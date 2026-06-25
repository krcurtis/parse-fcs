-- Copyright 2026 Fred Hutchinson Cancer Center
--------------------------------------------------------------------------------
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where



import qualified Options.Applicative as O
import Options.Applicative ((<|>))
import Data.String (IsString)
import Data.String.Interpolate



--------------------------------------------------------------------------------
-- local modules



--------------------------------------------------------------------------------
-- local data structures

data CommandParameters = SummarizeOptions { so_arg_input_fcs :: String
                                          }
                       deriving (Show)


--------------------------------------------------------------------------------

input_fcs_parse :: IsString s => O.Parser s
input_fcs_parse = O.strOption
                   ( O.long "input-fcs"
                   <> O.short 'f'
                   <> O.metavar "FILE"
                   <> O.help "flow cytometry/CyTOF file in FCS format")




summarize_opts :: O.Parser CommandParameters
summarize_opts = SummarizeOptions <$> input_fcs_parse


command_parameters :: O.Parser CommandParameters
command_parameters = O.hsubparser
                   ( O.command  "summarize"           (O.info summarize_opts (O.progDesc "Summarize FCS contents"))
--                   <> O.command "populate-assembly" (O.info assembly_opts (O.progDesc "Populate database with assembly jobs"))
                   )



opts :: O.ParserInfo CommandParameters
opts = O.info (command_parameters O.<**>  O.helper) O.idm

main :: IO ()
main = O.execParser opts >>= run_app


run_app :: CommandParameters -> IO ()
run_app SummarizeOptions{..}= do
  let arg_fcs_file = so_arg_input_fcs

  putStrLn [i|Reading #{arg_fcs_file}|]



    
