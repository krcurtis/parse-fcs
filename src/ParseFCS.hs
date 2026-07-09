-- Copyright 2026 Fred Hutchinson Cancer Center
-- Copyright 2020 Keith Curtis
--------------------------------------------------------------------------------
--- Parse FCS3.1 files

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}




{-
Keywords are in ASCII
Values are in UTF-8
address ranges seem to be of the inclusive type
byte offsets are from the beginning of a data set. The first data set starts at file offset zero. but there can be multiple data sets ...

delimiter could be 0x01 to 0x7e, but maybe is usually 0x2f , '/'?
-}





module ParseFCS where

import Data.Attoparsec.ByteString
import Data.Attoparsec.Combinator
import Data.Attoparsec.Binary
import Control.Applicative ((<*>), (*>), (<$>), (<|>), pure)



-- import qualified Data.ByteString as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C


import qualified Data.Text as T
import Data.Void
import System.IO

import Data.Bits.Utils (w82c, c2w8, w82s)
import Data.Char (isDigit)
import Data.Word -- for Word8

--------------------------------------------------------------------------------


-- I think there's some weird encoding options that one needs to worry about but this is the basic version
data FCSHeader = FCSHeader
    { fh_version :: B.ByteString
    , fh_text_start_offset :: Int
    , fh_text_last_offset :: Int
    , fh_data_start_offset :: Int
    , fh_data_last_offset :: Int
    , fh_analysis_start_offset :: Int
    , fh_analysis_last_offset :: Int
    , fh_other_offset :: Int
    }
  deriving (Show, Eq)

-- TODO define correct data structures
data FCS = FCS B.ByteString
  deriving (Show, Eq)

data FCSDataType = FCS_UNSIGNED_BINARY | FCS_FLOAT32 | FCS_FLOAT64 | FCS_ASCII
  deriving (Show, Eq)

data Parameter = Parameter { p_n_bits :: Int
                           , p_amplification :: (Float, Float)  -- values could be floats or integers sounds like, oh it may depend on the FCSDataType, yikes, like why
                           , p_name :: T.Text
                           , p_range :: Int  -- depend on the FCSDataType, yikes, like why
                           }
  deriving (Show, Eq)
                               

-- mandatory fields specifically listed, but this apparently requires a separate error checking step
data FCSText = FCSText { ft_analysis_start_offset :: Int
                       , ft_data_start_offset :: Int
                       , ft_supplemental_start_offset :: Int
                       , ft_byte_order :: Bool -- True if little endian, False if big endian
                       , ft_data_type :: FCSDataType
                       , ft_analysis_last_offset :: Int
                       , ft_data_last_offset :: Int
                       , ft_supplemental_last_offset :: Int
                       , ft_mode :: Bool
                       , ft_next_data_offset :: Int
                       , ft_n_parameters :: Int
                       , ft_parameters :: [Parameter]
                       , ft_n_total_events :: Int
                       , ft_optional :: [(T.Text,T.Text)]
                       }
  deriving (Show, Eq)


type ParameterBlob = [(T.Text, T.Text)] -- this might be the most general form for the TEXT segment


data FCSData = FCSData B.ByteString
  deriving (Show, Eq)

data FCSAnalysis = FCSAnalysis B.ByteString
  deriving (Show, Eq)

data FCSOther = FCSOther B.ByteString
  deriving (Show, Eq)

--------------------------------------------------------------------------------


fcs_version_length = 6
fcs_space = 4
fcs_text_first_length = 17 - 10 + 1
fcs_text_last_length = 25 - 10 + 1
fcs_data_header_first_length = 33 - 26 + 1
fcs_data_header_last_length = 41 - 34 + 1
fcs_analysis_header_first_length = 49-42 + 1
fcs_analysis_header_last_length = 57-50+1
fcs_base_header_length = fcs_version_length + fcs_space + fcs_text_first_length + fcs_text_last_length + fcs_data_header_first_length + fcs_data_header_last_length +fcs_analysis_header_first_length + fcs_analysis_header_last_length




load_fcs_base_header :: FilePath -> IO C.ByteString
load_fcs_base_header filename = do
    handle <- openFile filename ReadMode
    header_bytes <- C.hGet handle fcs_base_header_length
    hClose handle
    return header_bytes


-- TODO
{-
parse_fcs_base_header :: C.ByteString -> Either String FCSHeader
parse_fcs_base_header bytes | C.length bytes < fcs_base_header_length = Left "Insufficient bytes in header"
parse_fcs_base_header bytes = results
  where
    feed_thru :: Int -> C.ByteString -> (C.ByteString, C.ByteString)
    feed_thru n bytes = (C.take n, C.drop n)
    let (version_bytes, next) = feed_thru fcs_version_length bytes
        (_, next1) = feed_thru fcs_space next
        (text_first_start_bytes, next2) = feed_thru fcs_text_first_length next1
        (text_first_last_bytes, next3) = feed_thru fcs_text_last_length next2
        (text_last_start_bytes, next4) = feed_thru fcs_text_first_length next3
        (text_last_last_bytes, next5) = feed_thru fcs_text_last_length next4

        (data_first_start_bytes, next6) = feed_thru fcs_data_first_length next5
        (data_first_last_bytes, next7) = feed_thru fcs_data_last_length next6
        (data_last_start_bytes, next8) = feed_thru fcs_data_first_length next7
        (data_last_last_bytes, next9) = feed_thru fcs_data_last_length next8

        (analysis_first_start_bytes, next10) = feed_thru fcs_analysis_first_length next9
        (analysis_first_last_bytes, next11) = feed_thru fcs_analysis_last_length next10
        (analysis_last_start_bytes, next12) = feed_thru fcs_analysis_first_length next11
        (analysis_last_last_bytes, _) = feed_thru fcs_analysis_last_length next12
      in
      undefined

    results = undefined
-}


parse_fcs :: Parser FCS
parse_fcs = do
  parse_fcs_header
  parse_text_segment
  parse_data_segment
  parse_analysis_segment
  parse_other_segment -- maybe there could be multiple other segments?
  return $ FCS ""


{- the segments are at byte offsets, could the segments be at essentially random positions?? Do I need to load particular byte regions before parsing? -}

parse_right_justified_int :: Int -> Parser Int
parse_right_justified_int n = do
  pre_space <- takeTill (\c -> c /= c2w8 ' ')
  let n_digits  = n - B.length pre_space
  text_number <- count n_digits (satisfy (isDigit . w82c))
  let value = (read . w82s $ text_number) :: Int
  return value

-- TODO parse_spaces_as_zero :: Int -> Parser Int   -- I think OTHER segment offset is sometimes not given, and 

parse_fcs_header :: Parser FCSHeader
parse_fcs_header = do
  fh_version <- string "FCS3.1"
  _ <- string "    "
  
  fh_text_start_offset <- parse_right_justified_int 8
  fh_text_last_offset <- parse_right_justified_int 8
  fh_data_start_offset <- parse_right_justified_int 8
  fh_data_last_offset <- parse_right_justified_int 8
  fh_analysis_start_offset <- parse_right_justified_int 8
  fh_analysis_last_offset <- parse_right_justified_int 8
      
  let fh_other_offset = 0
  return FCSHeader{..}






-- required ??

keyword_begin_analsis =  "$BEGINANALYSIS" -- Byte-offset to the beginning of the ANALYSIS segment
keyword_begin_data = "$BEGINDATA" -- Byte-offset to the beginning of the DATA segment
keyword_begin_sup_text = "$BEGINSTEXT" -- Byte-offset to the beginning of a supplemental TEXT segment
keyword_byte_order = "$BYTEORD" -- Byte order for data acquisition computer
keyword_data_type = "$DATATYPE" -- Type of data in DATA segment (ASCII, integer, floating point)
keyword_end_analysis = "$ENDANALYSIS" -- Byte-offset to the last byte of the ANALYSIS segment
keyword_end_data = "$ENDDATA" -- Byte-offset to the last byte of the DATA segment
keyword_end_sup_text = "$ENDSTEXT" -- Byte-offset to the last byte of a supplemental TEXT segment
keyword_mode = "$MODE" -- Data mode (list mode - preferred, histogram - deprecated).
keyword_next_data = "$NEXTDATA" -- Byte offset to next data set in the file
keyword_n_params = "$PAR" -- Number of parameters in an event
keyword_param_bits = "$PnB" -- Number of bits reserved for parameter number n
keyword_amplify_param = "$PnE" -- Amplification type for parameter n
keyword_param_name = "$PnN" -- Short name for parameter n
keyword_param_range = "$PnR" -- Range for parameter number n
keyword_total_events = "$TOT" -- Total number of events in the data set

-- optional

keyword_abort = "$ABRT" -- Events lost due to data acquisition electronic coincidence
keyword_begin_clock = "$BTIM" -- Clock time at beginning of data acquisition
keyword_cell_desc = "$CELLS" -- Description of objects measured.
keyword_comment = "$COM" -- Comment
keyword_subset_mode = "$CSMODE" -- Cell subset mode, number of subsets to which an object may belong.
keyword_subset_bits = "$CSVBITS" -- Number of bits used to encode a cell subset identifier.
keyword_subset_flag = "$CSVnFLAG" -- The bit set as a flag for subset n.
keyword_cytometer_type = "$CYT" -- Type of flow cytometer
keyword_serial_number = "$CYTSN" -- Flow cytometer serial number
keyword_date = "$DATE" -- Date of data set acquisition
keyword_end_clock = "$ETIM" -- Clock time at end of data acquisition



keyword_investigator = "$EXP" -- Name of investigator initiating the experiment
keyword_file = "$FIL" -- Name of the data file containing the data set -- really? why?
keyword_gate = "$GATE" -- Number of gating parameters
keyword_gating = "$GATING" -- Specifies region combinations used for gating
keyword_amp_type = "$GnE" -- Amplification type for gating parameter number n (deprecated)
keyword_filter = "$GnF" -- Optical filter used for gating parameter number n (deprecated)
keyword_gate_name = "$GnN" -- Name of gating parameter number n (deprecated).
keyword_gate_percent = "$GnP" -- Percent of emitted light collected by gating parameter n (deprecated).
keyword_gate_range = "$GnR" -- Range of gating parameter n (deprecated)
keyword_gate_sname = "$GnS" -- Name used for gating parameter n (deprecated)
keyword_gate_type = "$GnT" -- Detector type for gating parameter n (deprecated)
keyword_gate_voltage = "$GnV" -- Detector voltage for gating parameter n (deprecated)
keyword_institution = "$INST" -- Institution at which data was acquired
keyword_last_modified = "$LAST_MODIFIED" -- Timestamp of the last modification of the data set.
keyword_last_person = "$LAST_MODIFIER" -- Name of the person performing last modification of a data set
keyword_lost = "$LOST" -- Number of events lost due to computer busy
keyword_operator = "$OP" -- Name of flow cytometry operator
keyword_original = "$ORIGINALITY" -- Information whether the FCS data set has been modified (any part of it) or is original as acquired by the instrument
keyword_peak_chan = "$PKn" -- Peak channel number of univariate histogram for parameter n (deprecated)
keyword_peak_count = "$PKNn" -- Count in peak channel of univariate histogram for parameter n (deprecated)

keyword_plate_id = "$PLATEID" -- Plate identifier
keyword_plate_name = "$PLATENAME" -- Plate name -- why?
keyword_param_calibration = "$PnCALIBRATION" -- Conversion of parameter values to any well defined units, e.g., MESF.
keyword_param_scale = "$PnD" -- Suggested visualization scale for parameter n
keyword_param_filter = "$PnF" -- Name of optical filter for parameter n
keyword_param_gain = "$PnG" -- Amplifier gain used for acquisition of parameter n
keyword_param_wavelength = "$PnL" -- Excitation wavelength(s) for parameter n
keyword_param_power = "$PnO" -- Excitation power for parameter n
keyword_param_collected = "$PnP" -- Percent of emitted light collected by parameter n
keyword_param_sname = "$PnS" -- Name used for parameter n
keyword_param_detector = "$PnT" -- Detector type for parameter n
keyword_param_voltage = "$PnV" -- Detector voltage for parameter n
keyword_project = "$PROJ" -- Name of the experiment project
keyword_param_gating = "$RnI" -- Gating region for parameter number n
keyword_gate_settings = "$RnW" -- Window settings for gating region n

keyword_specimen = "$SMNO" -- Specimen (e.g., tube) label
keyword_obs_matrix = "$SPILLOVER" -- Fluorescence spillover matrix -- the word spillover doesn't seem to really describe the mingled nature of the observation matrix
keyword_source = "$SRC" -- Source of the specimen (patient name, cell types) -- better not have PHI!
keyword_os = "$SYS" -- Type of computer and its operating system
keyword_time_step = "$TIMESTEP" -- Time step for time parameter
keyword_trigger = "$TR" -- Trigger parameter and its threshold
keyword_sample_volumen = "$VOL" -- Volume of sample run during data acquisition
keyword_well_id = "$WELLID" -- Well identifier


-- keywords are case insensitive, todo convert to upper case
parse_text_segment :: Parser ParameterBlob
parse_text_segment = do
  delimiter <- anyWord8  -- grab the arbitrary delimiter, expected to usually be '/' but spec allows this to be different
  return undefined

-- assume starts with $.../.../
parse_keyword_pair :: Word8 -> Parser (T.Text, T.Text)
parse_keyword_pair delimiter = undefined



parse_delimited_bytes :: Word8 -> Parser B.ByteString
parse_delimited_bytes delimiter = do
  bs <- Data.Attoparsec.ByteString.takeWhile (\x -> x /= delimiter)
  _ <- word8 delimiter
  escaped_delimiter <- peekWord8
  more_bs <- case escaped_delimiter of
               Just x | x == delimiter -> do   -- escaped delimiter
                                            _ <- word8 delimiter
                                            continued_bytes <- parse_delimited_bytes delimiter
                                            return $ delimiter `B.cons` continued_bytes
               Just _ | otherwise -> return ""
               Nothing -> return ""

  let bs' = bs `B.append` more_bs
  return bs'




parse_data_segment :: Parser FCSData
parse_data_segment = undefined

parse_analysis_segment :: Parser FCSAnalysis
parse_analysis_segment = undefined

parse_other_segment :: Parser FCSOther
parse_other_segment = undefined
