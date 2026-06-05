--------------------------------------------------------------------------------
--- Parse FCS3.1 files

{-

Keywords are in ASCII
Values are in UTF-8

address ranges seem to be of the inclusive type

byte offsets are from the beginning of a data set. The first data set starts at file offset zero. but there can be multiple data sets ...


delimiter could be 0x01 to 0x7e, but maybe is usually 0x2f , '/'?


I'm sure I'm going to want to throw exceptions about parse errors but I don't want the progam to crash in this during the test suite because of it.

-}



{-# LANGUAGE OverloadedStrings #-}


module ParseFCS where

import Text.Megaparsec
import Text.Megaparsec.Byte
import Data.Void
import qualified Data.ByteString.Strict as B
import qualified Data.ByteString.Char8 as C
import System.IO

--------------------------------------------------------------------------------

type Parser = Parsec Void ByteString


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
    header_bytes <- C.hget handle fcs_base_header_length
    hClose handle
    return header_bytes


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

        TODO

        

parseFCS :: Parser FCS
parseFCS = do
  parse_fcs_header
  parse_text_segment
  parse_data_segment
  parse_analysis_segment
  parse_other_segment -- maybe there could be multiple other segments?



{- the segments are at byte offsets, could the segments be at essentially random positions?? Do I need to load particular byte regions before parsing? -}

parse_fcs_header :: Parser FCSHeader
parse_fcs_header = do
    string "FCS3.1"
    string "    "
    text start
    text end
    data start
    data end
    analysis start
    analysis end
    other



-- keywords are case insensitive?
parse_text_segment :: Parser FCSText
parse_text_segment = undefined


parse_data_segment :: Parser FCSData
parse_data_segment = undefined

parse_analysis_segment :: Parser FCSAnalysis
parse_analysis_segment = undefined

parse_other_segment :: Parser FCSOther
parse_other_segment = undefined



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
keyword_param_sname = "$PnN" -- Short name for parameter n
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
keyword_param_wavelength = "$PnL" Excitation wavelength(s) for parameter n
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

