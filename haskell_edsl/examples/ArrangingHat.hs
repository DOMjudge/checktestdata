-- NWERC 2016 - Arranging Hat
-- See http://2016.nwerc.eu

module Main where

import Control.Monad
import Checktestdata

main :: IO ()
main = ctdMain $ do
  n <- int 1 40
  space
  m <- int 1 400
  newline
  replicateM_ (fromIntegral n) $ do
    replicateM_ (fromIntegral m) $ regex "[0-9]"
    newline
