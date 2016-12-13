module Main where

import Control.Monad
import Checktestdata

{-
Example script that reads an integer 'n' on the first line,
and then n lines with an integer between 0 and 100.
Unfortunately, example.in contains a negative number, so the
script fails and reports:

3\n4\n-7\n3\n
      ^^
Error in line 3 character 1
Value out of range in 0 <= -7 <= 100
-}

main :: IO ()
main = ctdMain $ do
  n <- int 1 100
  newline
  replicateM_ (fromIntegral n) $ do
    int 0 100
    newline


