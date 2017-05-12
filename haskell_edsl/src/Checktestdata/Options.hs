module Checktestdata.Options (
  FloatOption (..),
  ) where

--------------------------------------------------------------------------------
-- Some options
--------------------------------------------------------------------------------

-- | Which floating point notation is acceptable? @Fixed@ width (i.e. @23.534@),
--   @Scientific@ notation (i.e. @1.3e-2@) or @Both@.
data FloatOption
  = Both
  | Scientific
  | Fixed
  deriving (Show)
