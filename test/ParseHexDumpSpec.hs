--------------------------------------------------------------------------------
--- Test parsing of hexdump text to bytestrings



{-# LANGUAGE OverloadedStrings #-}

module ParseHexDumpSpec where

import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char

import Test.Hspec.Megaparsec

import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Word

--------------------------------------------------------------------------------

import ParseHexDump


spec :: Spec
spec = describe "Tests for parsing hexdump text" $ do


  it "parse hex byte" $ do
    let hex_text = "2e"
        expected = 0x2E :: Word8
    parse parse_hex_byte "" hex_text `shouldParse` expected

  it "parse hexdump line" $ do
    let hexdump_line = "00000000  46 43 53 33 2e 31 20 20  20 20 20 20 20 20 20 32  |FCS3.1         2|"
        -- expected = B.pack [0x46, 0x43, 0x53, 0x33, 0x2e, 0x31, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x32]
        expected = HexLine { hl_address = 0
                           , hl_bytes = [0x46, 0x43, 0x53, 0x33, 0x2e, 0x31, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x32]
                           , hl_ascii_text = "FCS3.1         2"
                           }
    parse parse_hexdump_line "" hexdump_line `shouldParse` expected


  it "parse bytes from one hexdump line" $ do
    let hexdump_text = T.pack . unlines $ [ "00000000  46 43 53 33 2e 31 20 20  20 20 20 20 20 20 20 32  |FCS3.1         2|"
                                          ]
        expected = B.pack [0x46, 0x43, 0x53, 0x33, 0x2e, 0x31, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x32]
    parse parse_hexdump "" hexdump_text `shouldParse` expected

  it "parse bytes from two hexdump line" $ do
    let hexdump_text = T.pack . unlines $ [ "00000000  46 43 53 33 2e 31 20 20  20 20 20 20 20 20 20 32  |FCS3.1         2|"
                                          , "00000010  35 36 20 20 20 20 31 36  38 31 20 20 20 20 31 36  |56    1681    16|"
                                          ]
        expected = B.pack [ 0x46, 0x43, 0x53, 0x33, 0x2e, 0x31, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x32
                          , 0x35, 0x36, 0x20, 0x20, 0x20, 0x20, 0x31, 0x36, 0x38, 0x31, 0x20, 0x20, 0x20, 0x20, 0x31, 0x36
                          ]
    parse parse_hexdump "" hexdump_text `shouldParse` expected


  it "parse hexdump via wrapper interface" $ do
    let hexdump_text = T.pack . unlines $ [ "00000000  46 43 53 33 2e 31 20 20  20 20 20 20 20 20 20 32  |FCS3.1         2|"
                                          , "00000010  35 36 20 20 20 20 31 36  38 31 20 20 20 20 31 36  |56    1681    16|"
                                          ]
        expected = B.pack [ 0x46, 0x43, 0x53, 0x33, 0x2e, 0x31, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x32
                          , 0x35, 0x36, 0x20, 0x20, 0x20, 0x20, 0x31, 0x36, 0x38, 0x31, 0x20, 0x20, 0x20, 0x20, 0x31, 0x36
                          ]
    (hexdump_to_bs hexdump_text) `shouldBe` expected


