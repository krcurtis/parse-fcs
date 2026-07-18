-- Copyright 2026 Fred Hutchinson Cancer Center
--------------------------------------------------------------------------------
--- Transform semi-structured information from low-level parsing of
--- FCS files into more structured record or array representations


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}


module TransformFCS where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Data.String.Interpolate

import qualified Data.Map.Strict as Map
import Data.Either (partitionEithers)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Set as Set
--import Data.Functor ((<&>))

-- import Data.Void
-- import System.IO

-- import Data.Bits.Utils (w82c, c2w8, w82s)
-- import Data.Char (isDigit)
-- import Data.Word -- for Word8
-- import Data.Text.Encoding (decodeUtf8, encodeUtf8)

--------------------------------------------------------------------------------
import ParseFCS



-- TODO define correct data structures
data FCS = FCS B.ByteString
  deriving (Show, Eq)

data FCSDataType = FCS_UNSIGNED_BINARY | FCS_FLOAT32 | FCS_FLOAT64 | FCS_ASCII
  deriving (Show, Eq)

data Parameter = Parameter { p_n_bits :: Int
                           , p_amplification :: (Float, Float)  -- values could be floats or integers sounds like, oh it may depend on the FCSDataType, yikes, like why
                           , p_name :: T.Text -- label typically including detector wavelength for flow cytometry , or something about the isotope used in the CyToF tag
                           , p_range :: Int  -- could also mean it was the ADC range if using FCS_UNSIGNED_BINARY and list mode
                           , p_sname :: Maybe T.Text -- label typically for the analyte or marker, like CD3e
                           }
  deriving (Show, Eq)


data FCSSegmentOffsets = FCSSegmentOffsets { fso_text_start_offset :: Int   -- supplemental text segments
                                           , fso_text_last_offset :: Int
                                           , fso_data_start_offset :: Int
                                           , fso_data_last_offset :: Int
                                           , fso_analysis_start_offset :: Int
                                           , fso_analysis_last_offset :: Int
                                           }
  deriving (Show, Eq)


-- mandatory fields specifically listed, but this apparently requires a separate error checking step
data FCSText = FCSText { ft_segment_offsets :: FCSSegmentOffsets
                       , ft_byte_order :: Bool -- True if little endian, False if big endian
                       , ft_data_type :: FCSDataType
                       , ft_list_mode :: Bool -- data acquisition mode, true if list mode, false if not. The other modes are deprecated in 3.1, so maybe not expected??
                       , ft_next_data_offset :: Int
                       , ft_n_parameters :: Int
                       , ft_parameters :: [Parameter]
                       , ft_n_total_events :: Int
                       , ft_key_values :: [(T.Text,T.Text)]  -- additional parameters not already decoded for this text segment
                       }
  deriving (Show, Eq)


-- required ??

-- keyword_begin_analsis =  "$BEGINANALYSIS" -- Byte-offset to the beginning of the ANALYSIS segment
-- keyword_begin_data = "$BEGINDATA" -- Byte-offset to the beginning of the DATA segment
-- keyword_begin_sup_text = "$BEGINSTEXT" -- Byte-offset to the beginning of a supplemental TEXT segment
-- keyword_byte_order = "$BYTEORD" -- Byte order for data acquisition computer
-- keyword_data_type = "$DATATYPE" -- Type of data in DATA segment (ASCII, integer, floating point)
-- keyword_end_analysis = "$ENDANALYSIS" -- Byte-offset to the last byte of the ANALYSIS segment
-- keyword_end_data = "$ENDDATA" -- Byte-offset to the last byte of the DATA segment
-- keyword_end_sup_text = "$ENDSTEXT" -- Byte-offset to the last byte of a supplemental TEXT segment
-- keyword_mode = "$MODE" -- Data mode (list mode - preferred, histogram - deprecated).
-- keyword_next_data = "$NEXTDATA" -- Byte offset to next data set in the file
-- keyword_n_params = "$PAR" -- Number of parameters in an event

keyword_param_bits :: Int -> T.Text
keyword_param_bits n = T.pack [i|$P#{n}B|] -- Number of bits reserved for parameter number n

keyword_amplify_param :: Int -> T.Text
keyword_amplify_param n = T.pack [i|$P#{n}E|] -- Amplification type for parameter n

keyword_param_name :: Int -> T.Text
keyword_param_name n = T.pack [i|$P#{n}N|] -- Short name for parameter n

keyword_param_range :: Int -> T.Text
keyword_param_range n = T.pack [i|$P#{n}R|] -- Range for parameter number n

keyword_total_events = "$TOT" -- Total number of events in the data set

-- optional

-- keyword_abort = "$ABRT" -- Events lost due to data acquisition electronic coincidence
-- keyword_begin_clock = "$BTIM" -- Clock time at beginning of data acquisition
-- keyword_cell_desc = "$CELLS" -- Description of objects measured.
-- keyword_comment = "$COM" -- Comment
-- keyword_subset_mode = "$CSMODE" -- Cell subset mode, number of subsets to which an object may belong.
-- keyword_subset_bits = "$CSVBITS" -- Number of bits used to encode a cell subset identifier.
-- keyword_subset_flag = "$CSVnFLAG" -- The bit set as a flag for subset n.
-- keyword_cytometer_type = "$CYT" -- Type of flow cytometer
-- keyword_serial_number = "$CYTSN" -- Flow cytometer serial number
-- keyword_date = "$DATE" -- Date of data set acquisition
-- keyword_end_clock = "$ETIM" -- Clock time at end of data acquisition



-- keyword_investigator = "$EXP" -- Name of investigator initiating the experiment
-- keyword_file = "$FIL" -- Name of the data file containing the data set -- really? why?
-- keyword_gate = "$GATE" -- Number of gating parameters
-- keyword_gating = "$GATING" -- Specifies region combinations used for gating
-- keyword_amp_type = "$GnE" -- Amplification type for gating parameter number n (deprecated)
-- keyword_filter = "$GnF" -- Optical filter used for gating parameter number n (deprecated)
-- keyword_gate_name = "$GnN" -- Name of gating parameter number n (deprecated).
-- keyword_gate_percent = "$GnP" -- Percent of emitted light collected by gating parameter n (deprecated).
-- keyword_gate_range = "$GnR" -- Range of gating parameter n (deprecated)
-- keyword_gate_sname = "$GnS" -- Name used for gating parameter n (deprecated)
-- keyword_gate_type = "$GnT" -- Detector type for gating parameter n (deprecated)
-- keyword_gate_voltage = "$GnV" -- Detector voltage for gating parameter n (deprecated)
-- keyword_institution = "$INST" -- Institution at which data was acquired
-- keyword_last_modified = "$LAST_MODIFIED" -- Timestamp of the last modification of the data set.
-- keyword_last_person = "$LAST_MODIFIER" -- Name of the person performing last modification of a data set
-- keyword_lost = "$LOST" -- Number of events lost due to computer busy
-- keyword_operator = "$OP" -- Name of flow cytometry operator
-- keyword_original = "$ORIGINALITY" -- Information whether the FCS data set has been modified (any part of it) or is original as acquired by the instrument
-- keyword_peak_chan = "$PKn" -- Peak channel number of univariate histogram for parameter n (deprecated)
-- keyword_peak_count = "$PKNn" -- Count in peak channel of univariate histogram for parameter n (deprecated)

-- keyword_plate_id = "$PLATEID" -- Plate identifier
-- keyword_plate_name = "$PLATENAME" -- Plate name -- why?
-- keyword_param_calibration = "$PnCALIBRATION" -- Conversion of parameter values to any well defined units, e.g., MESF.
-- keyword_param_scale = "$PnD" -- Suggested visualization scale for parameter n
-- keyword_param_filter = "$PnF" -- Name of optical filter for parameter n
-- keyword_param_gain = "$PnG" -- Amplifier gain used for acquisition of parameter n
-- keyword_param_wavelength = "$PnL" -- Excitation wavelength(s) for parameter n
-- keyword_param_power = "$PnO" -- Excitation power for parameter n
-- keyword_param_collected = "$PnP" -- Percent of emitted light collected by parameter n
-- keyword_param_sname = "$PnS" -- Name used for parameter n
keyword_param_sname :: Int -> T.Text
keyword_param_sname n = T.pack [i|$P#{n}S|] -- Short name for parameter n

-- keyword_param_detector = "$PnT" -- Detector type for parameter n
-- keyword_param_voltage = "$PnV" -- Detector voltage for parameter n
-- keyword_project = "$PROJ" -- Name of the experiment project
-- keyword_param_gating = "$RnI" -- Gating region for parameter number n
-- keyword_gate_settings = "$RnW" -- Window settings for gating region n

-- keyword_specimen = "$SMNO" -- Specimen (e.g., tube) label
-- keyword_obs_matrix = "$SPILLOVER" -- Fluorescence spillover matrix -- the word spillover doesn't seem to really describe the mingled nature of the observation matrix
-- keyword_source = "$SRC" -- Source of the specimen (patient name, cell types) -- better not have PHI!
-- keyword_os = "$SYS" -- Type of computer and its operating system
-- keyword_time_step = "$TIMESTEP" -- Time step for time parameter
-- keyword_trigger = "$TR" -- Trigger parameter and its threshold
-- keyword_sample_volumen = "$VOL" -- Volume of sample run during data acquisition
-- keyword_well_id = "$WELLID" -- Well identifier


--------------------------------------------------------------------------------


lookfor :: (Show k, Ord k) => Map.Map k v -> k -> Either String v
lookfor map k = case (Map.lookup k map) of
                 Nothing  -> Left [i|ERROR Missing expected entry #{show k}|]
                 Just val -> Right val


read_integer :: T.Text -> Int
read_integer x = read (T.unpack x)

read_float :: T.Text -> Float
read_float x = read (T.unpack x)


mandatory_parameters :: Int -> [T.Text]
mandatory_parameters n = basic ++ params
  where
    basic = [ "$BEGINSTEXT"
            , "$ENDSTEXT"
            , "$BEGINDATA"
            , "$ENDDATA"
            , "$BEGINANALYSIS"
            , "$ENDANALYSIS"
            , "$BYTEORD"
            , "$TOT"
            , "$DATATYPE"
            , "$MODE"
            , "$PAR"
            , "$NEXTDATA"
            ]
    params = concat [[keyword_param_bits i, keyword_amplify_param i, keyword_param_name i, keyword_param_range i] | i <- [1..n]]


read_amplification :: T.Text -> Either String (Float,Float)
read_amplification "0,0" = Right (0.0, 0.0)
read_amplification x = case (T.splitOn "," x) of
                         [a,b] -> Right (read_float a, read_float b)
                         _ -> Left [i|ERROR invalid string when trying to parse amplification #{x}|]



-- the amplification could integer "0,0" or floating point
lookup_parameter :: (Map.Map T.Text T.Text) -> Int -> Either String Parameter
lookup_parameter m i = do
  p_n_bits <- fmap read_integer $ lookfor m (keyword_param_bits i)
  p_amplification <- lookfor m (keyword_amplify_param i) >>= read_amplification
  p_name <- lookfor m (keyword_param_name i)
  p_range <- fmap read_integer $ lookfor m (keyword_param_range i)
  let p_sname = eitherToMaybe $ lookfor m (keyword_param_sname i)
  return Parameter{..}


transform_parameters :: [(T.Text, T.Text)] -> Either String FCSText
transform_parameters xs = do
  let m = Map.fromList xs

  fso_text_start_offset <- fmap read_integer $ lookfor m "$BEGINSTEXT"
  fso_text_last_offset <- fmap read_integer $ lookfor m "$ENDSTEXT"
  fso_data_start_offset  <- fmap read_integer $ lookfor m "$BEGINDATA"
  fso_data_last_offset  <- fmap read_integer $ lookfor m "$ENDDATA"
  fso_analysis_start_offset  <- fmap read_integer $ lookfor m "$BEGINANALYSIS"
  fso_analysis_last_offset <- fmap read_integer $ lookfor m "$ENDANALYSIS"
  let ft_segment_offsets = FCSSegmentOffsets{..}

  ft_n_total_events <- fmap read_integer $ lookfor m "$TOT"
  ft_next_data_offset <- fmap read_integer $ lookfor m "$NEXTDATA"
  ft_n_parameters <- fmap read_integer $ lookfor m "$PAR"
  ft_byte_order <- lookfor m "$BYTEORD" >>=   \x -> case x of
                                                     "1,2,3,4" -> Right True
                                                     "4,3,2,1" -> Right False
                                                     _ -> Left [i|ERROR byte order string not of allowed choices #{x}|]
  ft_data_type <- lookfor m "$DATATYPE" >>=   \x -> case x of
                                                     "I" -> Right FCS_UNSIGNED_BINARY
                                                     "F" -> Right FCS_FLOAT32
                                                     "D" -> Right FCS_FLOAT64
                                                     "A" -> Right FCS_ASCII
                                                     _ -> Left [i|ERROR data type not of allowed choices #{x}|]
  ft_list_mode <- lookfor m "$MODE" >>=   \x -> case x of
                                                 "L" -> Right True
                                                 "C" -> Right False
                                                 "U" -> Right False
                                                 _ -> Left [i|ERROR data acquisition mode not of allowed choices #{x}|]

  ft_parameters <- mapM (lookup_parameter m) [1 .. ft_n_parameters]

  
  let expected = mandatory_parameters ft_n_parameters
      expected' = Set.fromList expected
      ft_key_values = filter (\t -> fst t `Set.notMember` expected' ) xs
  return FCSText{..}
