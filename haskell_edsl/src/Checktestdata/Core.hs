{-# LANGUAGE GADTs #-}

module Checktestdata.Core (
  -- * Core representation
  CTD,
  runCTD,

  -- * Primitives
  peekChar,
  nextChar,
  nextNat,
  nextInt,
  nextHex,
  nextFloat,
  nextFloatP,
  string,
  regex,
  eof,
  isEOF,
  ) where

import Checktestdata.Options ( FloatOption (..) )

import Data.Char             ( isDigit, toUpper )
import Data.Maybe            ( fromMaybe, isJust )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Lex.Fractional as FR
import qualified Data.ByteString.Lex.Integral   as INT

import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

import Control.Monad.State
import Control.Monad.Trans.Either

--------------------------------------------------------------------------------
-- Main representation
--------------------------------------------------------------------------------

-- | Fields in the internal state
data InternalState = InternalState {
  -- | The full input
  full_input :: ByteString,
  -- | The remaining part of the input
  remaining  :: ByteString,
  -- | The length of the remaining part when we most recently started to read
  --   some input. This is used for error messages.
  last_start :: Int
  }

-- | Under the hood, this is our monad stack
type InternalMonad a = EitherT String (State InternalState) a

-- | The CheckTestData monad containing a deep embedding
--   of a testdata script.
data CTD a where
  Pure   :: a -> CTD a
  Apply  :: CTD (a -> b) -> CTD a -> CTD b
  Bind   :: CTD a -> (a -> CTD b) -> CTD b
  PrimOp :: InternalMonad a -> CTD a

instance Functor CTD where
  fmap f (Pure a)    = Pure (f a)
  fmap f (Apply a b) = Apply (fmap (f .) a) b
  fmap f (Bind a b)  = Bind a (fmap f . b)
  fmap f (PrimOp g)  = PrimOp (fmap f g)

instance Applicative CTD where
  pure  = Pure
  (<*>) = Apply

instance Monad CTD where
  return = Pure
  (>>=)  = Bind
  fail   = PrimOp . failWithLocation

-- | Run a checktestdata script on the given input and return either
--   an error or the result of the script. The 'eof' combinator is appended
--   to this script to ensure that the full input is consumed.
runCTD :: CTD a -> ByteString -> Either String a
runCTD sc inp = flip evalState initst $ runEitherT $ f $ sc <* eof where
  initst :: InternalState
  initst = InternalState inp inp 0
  f :: CTD a -> InternalMonad a
  f (Pure a)    = return a
  f (Apply a b) = f a <*> f b
  f (Bind a b)  = f a >>= f . b
  f (PrimOp g)  = g

--------------------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------------------

-- | Create an error message with information on where in the file
--   the error was found.
failWithLocation :: String -> InternalMonad a
failWithLocation e = do
  st <- get
  let inp = full_input st

  -- Compute error start and end indices
  let errS = BS.length inp - last_start st
  let errE = BS.length inp - BS.length (remaining st)
  
  -- Get 10 chars before the erroneous part
  let full_pref = BS.take errS inp
  let pref = showNoQuotes $ BS.reverse $ BS.take 10 $ BS.reverse full_pref

  -- Erroneous part
  let err  = showNoQuotes $ BS.take (errE - errS) $ BS.drop errS inp
  
  -- Get 10 chars after the erroneous part
  let suff = showNoQuotes $ BS.take 10 $ BS.drop errE inp

  -- Find line and character number
  let linenum = 1 + BS.count '\n' full_pref
  let charnum = 1 + BS.length full_pref - maybe 0 (+1) (BS.elemIndexEnd '\n' full_pref)

  -- Construct the error message
  left $ unlines $ [
    -- Part of the input
    pref ++ err ++ suff,
    -- Location
    replicate (length pref) ' ' ++ replicate (1 `max` length err) '^',
    -- The location in bytes
    "Error in line " ++ show linenum ++ " character " ++ show charnum,
    -- The error message
    e]

-- | Helper for proper error messages
showNoQuotes :: ByteString -> String
showNoQuotes = init . tail . show

--------------------------------------------------------------------------------
-- Primitives
--------------------------------------------------------------------------------

-- | Look at the next character from the input without consuming it.
peekChar :: CTD (Maybe Char)
peekChar = PrimOp $ do
  st <- getRemaining
  case BS.uncons st of
    Nothing    -> return $ Nothing
    Just (c,_) -> return $ Just c

-- | Get the next character from the input.
nextChar :: CTD Char
nextChar = PrimOp $ do
  st <- getRemaining
  case BS.uncons st of
    Nothing     -> failWithLocation "End of file"
    Just (c,cs) -> putRemaining cs >> return c

-- | Get next item from the input that can be parsed
--   with the given reader.
nextReader :: String -> (ByteString -> Maybe (a, ByteString)) -> CTD a
nextReader msg reader = PrimOp $ do
  cs <- getRemaining
  case reader cs of
    Nothing    -> failWithLocation msg
    Just (n,r) -> putRemaining r >> return n

-- | Get the next non-negative integer from the input.
nextNat :: CTD Integer
nextNat = PrimOp $ do
  -- Take prefix of the digits
  cs <- getRemaining
  let (num,rest) = BS.span isDigit cs

  -- Check validity
  when (BS.null num) $
    failWithLocation "Integer expected"
  when (BS.length num >= 2 && BS.head num == '0') $
    failWithLocation "Prefix zero(s)"

  -- Use efficient library for converting to actual integer
  putRemaining rest
  return $ INT.readDecimal_ num

-- | Get the next integer from the input. Accepts format (0|-?[1-9][0-9]*),
--   i.e. no leading zero's and no '-0' accepted.
nextInt :: CTD Integer
nextInt = do
  mbC <- peekChar
  case mbC of
    Just '-' -> do
      '-' <- nextChar
      i <- nextNat
      when (i == 0) $ fail "Invalid minus sign (-0 not allowed)"
      return (-i)
    _        -> nextNat

-- | Get the next hexadecimal number from the input.
nextHex :: CTD Integer
nextHex = nextReader "Could not parse as hexadecimal" $ INT.readSigned INT.readHexadecimal

-- | Helper for checking the float format
checkFloatFormat :: FloatOption -> ByteString -> InternalMonad ()
checkFloatFormat format digs = res where
  hasexp = isJust $ BS.findIndex (\x -> toUpper x == 'E') digs
  res = case format of
    Scientific | not hasexp -> failWithLocation "Scientific notation expected"
    Fixed      | hasexp     -> failWithLocation "Fixed notation expected"
    _                       -> return ()

-- | Get the next floating point number from the input in the given format.
nextFloat :: FloatOption -> CTD Rational
nextFloat format = PrimOp $ do
  cs <- getRemaining
  case FR.readSigned FR.readExponential cs of
    Nothing -> failWithLocation "Could not parse as floating point"
    Just (n,r) -> do
      putRemaining r
      let digs = BS.take (BS.length cs - BS.length r) cs
      checkFloatFormat format digs
      return n

-- | Get the next floating point number from the input in the given format,
--   with a number of decimals in the given range.
nextFloatP :: Int -> Int -> FloatOption -> CTD Rational
nextFloatP pmin pmax format = PrimOp $ do
  cs <- getRemaining
  case FR.readSigned FR.readExponential cs of
    Nothing -> failWithLocation "Could not parse as floating point"
    Just (n,r) -> do
      putRemaining r
      -- Get the matching string and check number of decimal digits
      let sn = BS.take (BS.length cs - BS.length r) cs
      let digitpos = fromMaybe (error "digitpos: impossible") $ BS.findIndex isDigit sn
      let dotpos = BS.elemIndex '.' sn
      let exppos = BS.findIndex (\x -> toUpper x == 'E') sn
      let hasexp = isJust exppos
      let ndecimals = case dotpos of
            Nothing -> 0
            Just i  -> fromMaybe (BS.length sn) exppos - i - 1
      case dotpos of
        Just dp | hasexp && (BS.index sn digitpos == '0' || dp /= digitpos + 1) ->
                    failWithLocation "exactly one non-zero before the decimal dot expected"
        _ -> return ()
      when (ndecimals < pmin || ndecimals > pmax) $
        failWithLocation $ "Number of decimals (" ++ show ndecimals ++ ") not "
                           ++ "within specified range " ++ show (pmin,pmax)
      checkFloatFormat format sn

      -- Return the result
      return n

-- | Match the given literal string
string :: String -> CTD ()
string s = PrimOp $ do
  cs <- getRemaining
  case BS.isPrefixOf (BS.pack s) cs of
    False -> failWithLocation $ "Expected " ++ show s
    True  -> do
      putRemaining $ BS.drop (length s) cs

-- | Match with the given regular expression
regex :: String -> CTD String
regex rs = PrimOp $ do
  let reg = compile blankCompOpt defaultExecOpt $ BS.pack rs
  case reg of
    Left e  -> failWithLocation e
    Right r -> do
      cs <- getRemaining
      let err = "Expression " ++ show rs ++ " does not match"
      case regexec r cs of
        Right (Just (pre,main,post,_)) -> case BS.null pre of
          True  -> do
            putRemaining post
            return $ BS.unpack main
          False -> failWithLocation err
        Left e  -> failWithLocation e
        Right _ -> failWithLocation err

-- | Check whether we are at the end of the file.
isEOF :: CTD Bool
isEOF = PrimOp $ do
  st <- getRemaining
  return $ BS.null st

-- | Assure that we are at the end of the file
eof :: CTD ()
eof = do
  isE <- isEOF
  when (not isE) $
    fail "EOF expected"

--------------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------------

putRemaining :: ByteString -> InternalMonad ()
putRemaining rm = do
  modify $ \st -> st { remaining = rm }

getRemaining :: InternalMonad ByteString
getRemaining = do
  st <- get
  -- Update last_start, which we use for error messages
  put $ st { last_start = BS.length $ remaining st }
  return $ remaining st
