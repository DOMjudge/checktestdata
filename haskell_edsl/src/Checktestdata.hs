module Checktestdata (
  -- * Core representation
  CTD,
  runCTD,

  -- * Main functionality
  ctdMain,
  runCTDFile,

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

import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess)
import System.IO          ( hPutStrLn, stderr)

import qualified Data.ByteString.Char8 as BS

-- | Run a checktestdata script on a file
runCTDFile :: Options -> CTD a -> FilePath -> IO (Either String a)
runCTDFile opts sc fp = do
  f <- BS.readFile fp
  return $ runCTD opts sc f

-- | Main function that reads the commandline arguments
--   and takes either a filename or reads from stdin.
ctdMain :: CTD a -> IO ()
ctdMain sc = do
  args <- getArgs
  -- todo: add -w options to commandline arguments
  bs <- case args of
    []    -> BS.getContents
    ["-"] -> BS.getContents
    [fp]  -> BS.readFile fp
    _     -> do
      nm <- getProgName
      putStrLn $ "Usage:"
      putStrLn $ "  " ++ nm ++ " data.in"
      putStrLn $ "  " ++ nm ++ " < data.in"
      exitFailure
  case runCTD defaultOptions sc bs of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right _ -> do
      putStrLn "Testdata OK"
      exitSuccess
