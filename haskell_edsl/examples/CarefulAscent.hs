-- NWERC 2016 - Careful Ascent
-- See http://2016.nwerc.eu

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Control.Monad
import Checktestdata

main :: IO ()
main = ctdMain $ do
  x <- int (-(10^7)) (10^7)
  space
  y <- int (max 1 (abs x)) (10^8)
  newline

  n <- int 0 100
  newline

  shields <- forM [1..n] $ \i -> do
    li <- int 0 y
    space
    ui <- int (li+1) y
    space
    fi <- floatP 0.1 10.0 0 10
    newline
    return (i,li,ui,fi)

  -- Check for each pair that they don't intersect
  assert $ and $
    [ i == j || ui <= lj || uj <= li
    | (i,li,ui,_) <- shields
    , (j,lj,uj,_) <- shields
    ]
