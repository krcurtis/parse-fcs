--------------------------------------------------------------------------------
-- Test parsing of FCS files

{-# LANGUAGE OverloadedStrings #-}

module ParseFCSSpec where

import Test.Hspec
import Test.Hspec.Attoparsec

--------------------------------------------------------------------------------
import qualified Data.Text as T
import qualified Data.ByteString as B
import ParseFCS

hexdump = T.pack . unlines $ [ "00000000  46 43 53 33 2e 31 20 20  20 20 20 20 20 20 20 32  |FCS3.1         2|"
                             , "00000010  35 36 20 20 20 20 31 36  38 31 20 20 20 20 31 36  |56    1681    16|"
                             , "00000020  38 32 20 20 20 20 34 35  36 31 20 20 20 20 20 20  |82    4561      |"
                             , "00000030  20 30 20 20 20 20 20 20  20 30 20 20 20 20 20 20  | 0       0      |"
                             , "00000040  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             , "00000050  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             , "00000060  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             , "00000070  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             , "00000080  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             , "00000090  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             , "000000a0  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             , "000000b0  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             , "000000c0  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             , "000000d0  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             , "000000e0  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             , "000000f0  20 20 20 20 20 20 20 20  20 20 20 20 20 20 20 20  |                |"
                             ]


spec :: Spec
spec = describe "Tests for parsing FCS format" $ do

  it "parse only spaces" $ do
    let bytes = B.pack [0x48]
        expected = FCSHeader { fh_version = "3.1"
                             , fh_text_start_offset = 256
                             , fh_text_last_offset = 1681
                             , fh_data_start_offset = 1682
                             , fh_data_last_offset = 4561
                             , fh_analysis_start_offset = 0
                             , fh_analysis_last_offset = 0
                             , fh_other_offset = 0
                             }
    bytes ~> parse_fcs_header `shouldParse` expected



