-- Copyright 2026 Fred Hutchinson Cancer Center
--------------------------------------------------------------------------------
--- Parse Hex dump files to ByteString


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module ParseHexDump where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Pos
import Text.Megaparsec.Error -- for errorBundlePretty
import Data.Void -- for Void type

import Data.Functor ((<&>))
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M


import Data.Char (isSpace, isDigit)

-- import Data.List
-- Data.Char or Data.Word8

-- isHexDigit

import qualified Data.ByteString as B
import Data.Word

--------------------------------------------------------------------------------

data HexLine = HexLine { hl_address :: Word32
                       , hl_bytes :: [Word8]
                       , hl_ascii_text :: T.Text
                       }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
type Parser = Parsec Void T.Text




field_digits :: Parser T.Text
field_digits = do
  field <- takeWhile1P (Just "field digits") isDigit
  return field



parse_hex_byte :: Parser Word8
parse_hex_byte = do
  two_digits <- count 2 hexDigitChar
  let hex_value = (read $ ['0', 'x'] ++ two_digits) :: Word8
  return hex_value

parse_hex_32bit :: Parser Word32
parse_hex_32bit = do
  eight_digits <- count 8 hexDigitChar
  let hex_value = (read $ ['0', 'x'] ++ eight_digits) :: Word32
  return hex_value


parse_hexdump_line :: Parser HexLine
parse_hexdump_line = do
  hl_address <- parse_hex_32bit
  _ <- string "  "
  first_eight <- count 8 (parse_hex_byte <* char ' ')
  _ <- char ' '
  second_eight <- count 8 (parse_hex_byte <* char ' ')
  _ <- string " |"
  hl_ascii_text <- fmap T.pack (count 16 anySingle)
  _ <- char '|'
  let hl_bytes = first_eight ++ second_eight
  return $ HexLine{..}
  


parse_hexdump :: Parser B.ByteString
parse_hexdump = do
  lines <- some (parse_hexdump_line <* newline)
  let bss = map hl_bytes lines
      bs = concat bss
      results = B.pack bs
  return results
