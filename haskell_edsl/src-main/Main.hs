module Main where

import Checktestdata
import Checktestdata.Script

import System.Environment ( getArgs, withArgs, getProgName )
import System.Exit        ( exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []   -> printUsage
    (fp:rargs) -> do
      ast <- parseScript fp
      withArgs rargs $ ctdMain $ interpret ast

printUsage :: IO ()
printUsage = do
      nm <- getProgName
      putStrLn $ "Usage:"
      putStrLn $ "  " ++ nm ++ " script.ctd data.in"
      putStrLn $ "  " ++ nm ++ " script.ctd < data.in"
      exitFailure
