-- Copyright 2026 Fred Hutchinson Cancer Center
--------------------------------------------------------------------------------
--- Parse Hex dump files to ByteString


{-# LANGUAGE OverloadedStrings #-}

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

import Data.ByteString as B
import Data.Word

--------------------------------------------------------------------------------

type Parser = Parsec Void T.Text


field_digits :: Parser T.Text
field_digits = do
  field <- takeWhile1P (Just "field digits") isDigit
  return field



parse_hex_byte :: Parser Word8
parse_hex_byte = do
  return undefined

parse_hexdump_line :: Parser B.ByteString
parse_hexdump_line = do
  return undefined
  
