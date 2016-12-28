module Checktestdata.Derived (
  -- * Input readers
  int,
  float,
  space,
  newline,

  -- * Utilities
  match,
  assert,
  unique ) where

import Checktestdata.Core
import Control.Monad      ( when )
import qualified Data.Set as Set
import Numeric

--------------------------------------------------------------------------------
-- Input readers
--------------------------------------------------------------------------------

-- | Get the next integer which should be between the given lower
--   and upper bound (inclusive).
int :: Integer -> Integer -> CTD Integer
int low high = do
  i <- nextInt
  when (i < low || i > high) $
    fail $ "Value out of range in "
      ++ show low ++ " <= " ++ show i ++ " <= " ++ show high
  return i

-- | Get the next float which should be between the given lower
--   and upper bound (inclusive).
float :: Rational -> Rational -> CTD Rational
float low high = do
  i <- nextFloat
  when (i < low || i > high) $
    fail $ "Value out of range in "
      ++ showF low ++ " <= " ++ showF i ++ " <= " ++ showF high
  return i

showF :: Rational -> String
showF x = show (fromRat x :: Double)

-- | Check that the next character is a space.
space :: CTD ()
space = do
  c <- nextChar
  when (c /= ' ') $
    fail $ "Space expected"

-- | Check that the next character is a newline.
newline :: CTD ()
newline = do
  c <- nextChar
  when (c /= '\n') $
    fail $ "Newline expected"

-- | Match any of the given characters
match :: String -> CTD Bool
match s = do
  mbC <- peekChar
  case mbC of
    Just c | c `elem` s -> return True
    _                   -> return False

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Check that the given list contains distinct
--   elements.
unique :: Ord a => [a] -> Bool
unique xs = Set.size (Set.fromList xs) == length xs

-- | Make sure that the given argument is true.
assert :: Bool -> CTD ()
assert True  = return ()
assert False = fail $ "Assertion failed"
