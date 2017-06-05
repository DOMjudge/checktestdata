module Checktestdata (
  -- * Core representation
  CTD,
  runCTD,

  -- * Main functionality
  ctdMain,
  ctdMainOpts,

  -- * Commandline parsers
  generalOpts,

  -- * Primitives
  peekChar,
  nextChar,
  nextNat,
  nextHex,
  nextFloat,
  
  -- * Input readers
  nextInt,
  int,
  float,
  space,
  newline,

  -- * Utilities
  eof,
  isEOF,
  assert,
  unique) where

import Checktestdata.Core
import Checktestdata.Derived
import Checktestdata.Options

import System.Exit        ( exitFailure )
import System.IO          ( hPutStrLn, stderr)

import Control.Monad      ( when )

import Options.Applicative
import Data.Semigroup ((<>))

import qualified Data.ByteString.Char8 as BS

-- | Main function that reads the commandline arguments
--   and takes either a filename or reads from stdin.
ctdMain :: CTD a -> IO ()
ctdMain sc = do
  opts <- execParser $ info (generalOpts <**> helper)
    ( fullDesc
      <> progDesc "Check the data for testdata.in"
      <> header "checktestdata" )
  ctdMainOpts opts sc

-- | Main function that reads the input file given in the options.
ctdMainOpts :: Options -> CTD a -> IO ()
ctdMainOpts opts sc = do
  bs <- case input_file opts of
    Nothing  -> BS.getContents
    Just "-" -> BS.getContents
    Just fp  -> BS.readFile fp
  case runCTD opts sc bs of
    Left err -> do
      when (not $ quiet opts) $
        hPutStrLn stderr err
      exitFailure
    Right _ -> do
      when (not $ quiet opts) $
        putStrLn "Testdata OK"


--------------------------------------------------------------------------------
-- Command line option parsing
--------------------------------------------------------------------------------

-- | Parser for the general commandline options.
generalOpts :: Parser Options
generalOpts = Options
  <$> switch
  ( long "whitespace-ok"
    <> short 'w'
    <> help "whitespace changes are accepted, including heading and trailing whitespace, but not newlines; be careful: extra whitespace matches greedily!" )
  <*> switch
  ( long "quiet"
    <> short 'q'
    <> help "don't display testdata error messages: test exitcode" )
  <*> optional (argument str (metavar "testdata.in"))

  
