-- NWERC 2016 - Free Weights
-- See http://2016.nwerc.eu

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Control.Monad
import Checktestdata
import Data.List

main :: IO ()
main = ctdMain $ do
  n <- int 1 (10^6)
  newline

  row1 <- repSep n space $ int 1 (10^9)
  newline
  row2 <- repSep n space $ int 1 (10^9)
  newline

  -- Check that each number appears exactly twice.
  -- Numbers at odd positions should be equal to
  -- the next number in the list, and numbers at
  -- even positions should be smaller than the next.
  let ws = sort $ row1 ++ row2
  let check (i,v,w) | odd i     = v == w
                    | otherwise = v < w
  assert $ all check $ zip3 [1..] ws (tail ws)

-- | Replicate the given script n times, separated by
--   something else.
repSep :: Integer -> CTD () -> CTD a -> CTD [a]
repSep n sep a = forM [1..n] $ \i -> when (i > 1) sep >> a
