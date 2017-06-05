module Main where

import Checktestdata
import Checktestdata.Script
import Checktestdata.Options

import Options.Applicative
import Data.Semigroup ((<>))


main :: IO ()
main = do
  opts <- execParser $ info (mainOpts <**> helper)
    ( fullDesc
      <> progDesc "Check the data for testdata.in with script.ctd"
      <> header "checktestdata" )
  ast <- parseScript $ script_file opts
  ctdMainOpts (script_options opts) $ interpret ast


-- | Parser for the commandline options of a checktestdata script.
mainOpts :: Parser MainOptions
mainOpts = MainOptions
  <$> argument str (metavar "script.ctd")
  <*> generalOpts


data MainOptions = MainOptions {
  script_file    :: FilePath,
  script_options :: Options
  }
