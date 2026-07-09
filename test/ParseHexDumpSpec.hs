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

  it "parse short hexdump" $ do
    let hexdump_text = T.pack . unlines $ [ "00000000  24 42 45 47 49 4e 2f 61  62 63 2f 2f 64 65 66 67  |$BEGIN/abc//defg|"
                                          , "00000010  68 69 6a 6b 6c 69 6d 6f  70 2f 0a                 |hijklimop/.|"
                                          ]
        expected = B.pack [ 0x24, 0x42, 0x45, 0x47, 0x49, 0x4e, 0x2f, 0x61,  0x62, 0x63, 0x2f, 0x2f, 0x64, 0x65, 0x66, 0x67
                          , 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x69, 0x6d, 0x6f,  0x70, 0x2f, 0x0a
                          ]
    (hexdump_to_bs hexdump_text) `shouldBe` expected



