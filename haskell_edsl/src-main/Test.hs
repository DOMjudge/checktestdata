module Main ( main ) where

import Checktestdata
import Checktestdata.Script
import Checktestdata.Options

import Control.Monad
import Control.Exception

import System.Directory ( listDirectory )
import Data.List        ( isPrefixOf, isSuffixOf )

import System.Exit      ( exitFailure )

testsdir :: String
testsdir = "../tests/"

main :: IO ()
main = do
  -- Read the tests directory
  allfiles <- listDirectory testsdir

  -- Generic function we can run for both whitespace and non-whitespace
  let tests opts progf datf = do
        -- Go over all regular test programs
        let isProg f = progf `isPrefixOf` f && ".in" `isSuffixOf` f
        r1 <- forM (filter isProg allfiles) $ \prog -> do
          -- Get the test num
          let testnum = takeWhile (/='.') $ drop (length progf) prog
    
          -- Go over the correct testdata files
          let isCorrect f = (datf ++ testnum ++ ".in") `isPrefixOf` f
          r2 <- forM (filter isCorrect allfiles) $ \dataf -> checkSuccess opts prog dataf
    
          -- Go over the failure testdata files
          let isFailure f = (datf ++ testnum ++ ".err") `isPrefixOf` f
          r3 <- forM (filter isFailure allfiles) $ \dataf -> checkFailure opts prog dataf

          return $ r2 ++ r3

        -- Go over all test programs that should fail
        let isErrProg f = progf `isPrefixOf` f && ".err" `isSuffixOf` f
        r4 <- forM (filter isErrProg allfiles) $ \prog -> do
          -- Get the test num
          let testnum = takeWhile (/='.') $ drop (length "testprog") prog
    
          -- Go over the correct testdata files
          let isCorrect f = (datf ++ testnum ++ ".in") `isPrefixOf` f
          forM (filter isCorrect allfiles) $ \dataf -> checkFailure opts prog dataf

        -- Return all results
        return $ concat $ r1 ++ r4

  -- Run tests for normal progs
  r1 <- tests defaultOptions "testprog" "testdata"

  -- Run tests with -w options
  r2 <- tests (defaultOptions { whitespace_ok = True }) "testwsprog" "testwsdata"

  -- Check that all tests succeeded
  when (not $ and $ r1 ++ r2) $ exitFailure
    
-- | Run the prog on the given data file and ensure that it succeeded.
checkSuccess :: Options -> FilePath -> FilePath -> IO Bool
checkSuccess opts prog dataf = do
  res <- checkRun opts prog dataf
  case res of
    Right () -> return True
    Left _   -> do
      putStrLn $ "Running " ++ prog ++ " on " ++ dataf ++ " did not succeed"
      return False

-- | Run the prog on the given data file and ensure that it failed
checkFailure :: Options -> FilePath -> FilePath -> IO Bool
checkFailure opts prog dataf = do
  res <- checkRun opts prog dataf
  case res of
    Left _   -> return True
    Right () -> do
      putStrLn $ "Running " ++ prog ++ " on " ++ dataf ++ " did not fail"
      return False

-- | Run the prog on the given data file and return it's success or failure
checkRun :: Options -> FilePath -> FilePath -> IO (Either String ())
checkRun opts prog dataf = do
  res <- try $ do
    ctd <- parseScript $ testsdir ++ prog
    runCTDFile opts (interpret ctd) $ testsdir ++ dataf
  case res of
    Left e -> return $ Left $ show (e :: SomeException)
    Right (Left e) -> return $ Left e
    Right (Right _) -> return $ Right ()
