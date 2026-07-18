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
import Control.Applicative ((<*>), (*>), (<$>), (<|>), pure, some)



-- import qualified Data.ByteString as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C


import qualified Data.Text as T
import Data.Void
import System.IO

import Data.Bits.Utils (w82c, c2w8, w82s)
import Data.Char (isDigit)
import Data.Word -- for Word8
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

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


-- type ParameterBlob = [(T.Text, T.Text)] -- this might be the most general form for the TEXT segment


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




{-
parse_fcs :: Parser FCS
parse_fcs = do
  parse_fcs_header
  parse_text_segment
  parse_data_segment
  parse_analysis_segment
  parse_other_segment -- maybe there could be multiple other segments?
  return $ FCS ""
-}

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
  fh_version <- (try (string "FCS3.1")) <|> (try (string "FCS3.0"))  -- maybe 3.0 still works?
  _ <- string "    "
  
  fh_text_start_offset <- parse_right_justified_int 8
  fh_text_last_offset <- parse_right_justified_int 8
  fh_data_start_offset <- parse_right_justified_int 8
  fh_data_last_offset <- parse_right_justified_int 8
  fh_analysis_start_offset <- parse_right_justified_int 8
  fh_analysis_last_offset <- parse_right_justified_int 8
      
  let fh_other_offset = 0
  return FCSHeader{..}







-- keywords are case insensitive, todo convert to upper case
parse_text_segment :: Parser [(T.Text, T.Text)]
parse_text_segment = do
  delimiter <- anyWord8  -- grab the arbitrary delimiter, expected to usually be '/' but spec allows this to be different
  pairs <- some (parse_keyword_pair delimiter)
  return pairs




-- assume starts with $.../.../
parse_keyword_pair :: Word8 -> Parser (T.Text, T.Text)
parse_keyword_pair delimiter = do
  keyword <- parse_delimited_bytes delimiter
  value <- parse_delimited_bytes delimiter

  let keyword' = T.toUpper . decodeUtf8 $ keyword
      value' = decodeUtf8 value
  return (keyword', value')


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
