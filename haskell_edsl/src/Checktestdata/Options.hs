module Checktestdata.Options (
  FloatOption (..),
  Options (..),
  defaultOptions
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

-- | The options the user can set via the commandline or programatically.
data Options = Options {
  -- | When set to 'True', whitespace changes are accepted, including heading
  --   and training whitespace, but not newlines. Be careful: extra whitespace
  --   matches greedily!
  whitespace_ok :: Bool
  }

-- | The default values for 'Options'
defaultOptions :: Options
defaultOptions = Options {
  whitespace_ok = False
  }
