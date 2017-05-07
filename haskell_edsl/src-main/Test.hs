module Main ( main ) where

import Checktestdata
import Checktestdata.Script

import Control.Monad
import Control.Exception

import System.Directory ( listDirectory )
import Data.List        ( isPrefixOf, isSuffixOf )


testsdir :: String
testsdir = "../tests/"

main :: IO ()
main = do
  -- Read the tests directory
  allfiles <- listDirectory testsdir

  -- Go over all regular test programs
  let isProg f = "testprog" `isPrefixOf` f && ".in" `isSuffixOf` f
  forM_ (filter isProg allfiles) $ \prog -> do
    -- Get the test num
    let testnum = takeWhile (/='.') $ drop (length "testprog") prog
    
    -- Go over the correct testdata files
    let isCorrect f = ("testdata"++ testnum ++ ".in") `isPrefixOf` f
    forM_ (filter isCorrect allfiles) $ \dataf -> checkSuccess prog dataf
    
    -- Go over the failure testdata files
    let isFailure f = ("testdata"++ testnum ++ ".err") `isPrefixOf` f
    forM_ (filter isFailure allfiles) $ \dataf -> checkFailure prog dataf

  -- Go over all test programs that should fail
  let isErrProg f = "testprog" `isPrefixOf` f && ".err" `isSuffixOf` f
  forM_ (filter isErrProg allfiles) $ \prog -> do
    -- Get the test num
    let testnum = takeWhile (/='.') $ drop (length "testprog") prog
    
    -- Go over the correct testdata files
    let isCorrect f = ("testdata"++ testnum ++ ".in") `isPrefixOf` f
    forM_ (filter isCorrect allfiles) $ \dataf -> checkFailure prog dataf

    
-- | Run the prog on the given data file and ensure that it succeeded.
checkSuccess :: FilePath -> FilePath -> IO ()
checkSuccess prog dataf = do
  res <- checkRun prog dataf
  case res of
    Right () -> return ()
    Left _   -> putStrLn $ "Running " ++ prog ++ " on " ++ dataf ++ " did not succeed"

-- | Run the prog on the given data file and ensure that it failed
checkFailure :: FilePath -> FilePath -> IO ()
checkFailure prog dataf = do
  res <- checkRun prog dataf
  case res of
    Right () -> putStrLn $ "Running " ++ prog ++ " on " ++ dataf ++ " did not fail"
    Left _   -> return ()

-- | Run the prog on the given data file and return it's success or failure
checkRun :: FilePath -> FilePath -> IO (Either String ())
checkRun prog dataf = do
  res <- try $ do
    ctd <- parseScript $ testsdir ++ prog
    runCTDFile (interpret ctd) $ testsdir ++ dataf
  case res of
    Left e -> return $ Left $ show (e :: SomeException)
    Right (Left e) -> return $ Left e
    Right (Right _) -> return $ Right ()
