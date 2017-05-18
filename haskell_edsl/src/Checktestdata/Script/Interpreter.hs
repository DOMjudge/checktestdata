{-# LANGUAGE RankNTypes #-}

module Checktestdata.Script.Interpreter (
  interpret
  ) where

import Checktestdata.Core
import Checktestdata.Derived
import Checktestdata.Script.AST

import Data.List ( genericLength, transpose )
import Data.Map ( Map )
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State

--------------------------------------------------------------------------------
-- Storing values
--------------------------------------------------------------------------------

-- todo: error handling on script parsing level

data Value = VInt    Integer
           | VFloat  Rational
           | VString String
           deriving ( Show, Eq, Ord )

type ValueMap = Map VarName (Map [Value] Value)

setValue :: Maybe Var -> Value -> InterpretMonad ()
setValue Nothing    _   = return ()
setValue (Just (Var var eidxs)) val = do
  idxs <- mapM fExpr eidxs
  modify $ Map.insertWith Map.union var $ Map.singleton idxs val

getValue :: Var -> InterpretMonad Value
getValue (Var var eidxs) = do
  vm <- get
  case Map.lookup var vm of
    Nothing  -> error $ "Undefined variable " ++ show var
    Just vmi -> do
      idxs <- mapM fExpr eidxs
      case Map.lookup idxs vmi of
        Nothing  -> error $ "Undefined index " ++ show idxs
        Just val -> return val

unsetVar :: VarName -> InterpretMonad ()
unsetVar var = modify $ Map.delete var

toInt :: Value -> InterpretMonad Integer
toInt (VInt i) = return i
toInt _ = error "Integer expected"

toFloat :: Value -> InterpretMonad Rational
toFloat (VInt i)   = return $ fromIntegral i
toFloat (VFloat i) = return i
toFloat  _         = error "Integer or float expected"

toString :: Value -> InterpretMonad String
toString (VString s) = return s
toString _           = error "String expected"

--------------------------------------------------------------------------------
-- Main interpreter
--------------------------------------------------------------------------------

type InterpretMonad a = StateT ValueMap CTD a

-- | Lift to the 'CTD' level
liftC :: CTD a -> InterpretMonad a
liftC = lift 

-- | Interpret an old checktestdata script into an executable 'CTD'
interpret :: Block -> CTD ()
interpret = flip evalStateT Map.empty . fBlock

  -- Code blocks (simple fold)
fBlock :: Block -> InterpretMonad ()
fBlock = mapM_ fAST

-- AST elements
fAST :: AST -> InterpretMonad ()
fAST CSpace   = liftC space
fAST CNewline = liftC newline
fAST CEOF     = liftC eof
fAST (CSet vs) = forM_ vs $ \(var,e) -> do
  val <- fExpr e
  setValue (Just var) val
fAST (CUnset vs) = mapM_ unsetVar vs
fAST (CInt low up var) = do
  vlow <- fExpr low >>= toInt
  vup <- fExpr up >>= toInt
  val <- liftC $ int vlow vup
  setValue var (VInt val)
fAST (CFloat low up var format) = do
  vlow <- fExpr low >>= toFloat
  vup <- fExpr up >>= toFloat
  val <- liftC $ floatOpt vlow vup format
  setValue var (VFloat val)
fAST (CFloatP low up pmin pmax var format) = do
  vlow <- fExpr low >>= toFloat
  vup <- fExpr up >>= toFloat
  vpmin <- fExpr pmin >>= toInt
  vpmax <- fExpr pmax >>= toInt
  val <- liftC $ floatPOpt vlow vup (fromInteger vpmin) (fromInteger vpmax) format
  setValue var (VFloat val)
fAST (CString s) = do
  str <- fExpr s >>= toString
  liftC $ string str
fAST (CRegex sr var) = do
  r <- fExpr sr >>= toString
  val <- liftC $ regex r
  setValue var (VString val)
fAST (CAssert test) = do
  b <- fTest test
  liftC $ assert b
fAST (CRep var count mbSep body) = do
  vcount <- fExpr count >>= toInt
  forM_ [0..vcount-1] $ \i -> do
    -- Parse separator
    case mbSep of
      Just sep | i > 0 -> fAST sep
      _ -> return ()
      
    -- Set iterator
    setValue var (VInt i)
    
    -- Do the body
    fBlock body
fAST (CWhile var test mbSep body) = do
  let it :: Integer -> InterpretMonad ()
      it i = do
        setValue var (VInt i)
        b <- fTest test
        when b $ do
          -- Condition true, so parse separator
          case mbSep of
            Just sep | i > 0 -> fAST sep
            _                -> return ()
          -- And parse body
          fBlock body
          -- And repeat
          it $ i + 1
  it 0
fAST (CIf test ifTrue mbIfFalse) = do
  b <- fTest test
  if b then fBlock ifTrue
       else case mbIfFalse of
              Nothing -> return ()
              Just bl -> fBlock bl

-- Expression evaluation
fExpr :: Expr -> InterpretMonad Value
fExpr (EVar var) = getValue var
fExpr (ConstI v) = return $ VInt v
fExpr (ConstF v) = return $ VFloat v
fExpr (ConstS v) = return $ VString v
fExpr (StrLen e) = do
  val <- fExpr e >>= toString
  return $ VInt $ genericLength val
fExpr (Negate e) = do
  val <- fExpr e
  case val of
    VInt v   -> return $ VInt   $ negate v
    VFloat v -> return $ VFloat $ negate v
    _        -> error "Integer of float expected"
fExpr  (BinOp op e1 e2) = do
  v1 <- fExpr e1
  v2 <- fExpr e2
  fBinOp op v1 v2

-- Binary operators
fBinOp :: BinOp -> Value -> Value -> InterpretMonad Value
fBinOp Plus  v1 v2 = fNumOp (+) v1 v2
fBinOp Minus v1 v2 = fNumOp (-) v1 v2
fBinOp Times v1 v2 = fNumOp (*) v1 v2
fBinOp Div   v1 v2 = case (v1, v2) of
  (VInt i1, VInt i2) -> return $ VInt $ i1 `div` i2
  _                  -> do
    f1 <- toFloat v1
    f2 <- toFloat v2
    return $ VFloat $ f1 / f2
fBinOp Modulo v1 v2 = do
  i1 <- toInt v1
  i2 <- toInt v2
  return $ VInt $ i1 `mod` i2
fBinOp Pow    v1 v2 = do
  i2 <- toInt v2
  case v1 of
    VInt   i1 -> return $ VInt   $ i1 ^ i2
    VFloat f1 -> return $ VFloat $ f1 ^^ i2
    _         -> error "Integer of float expected"

fNumOp :: (forall a. Num a => a -> a -> a) -> Value -> Value -> InterpretMonad Value
fNumOp op v1 v2 = case (v1, v2) of
  (VInt i1, VInt i2) -> return $ VInt $ i1 `op` i2
  _ -> do
    f1 <- toFloat v1
    f2 <- toFloat v2
    return $ VFloat $ f1 `op` f2

fTest :: Test -> InterpretMonad Bool
fTest IsEOF       = liftC isEOF
fTest (Not test)  = liftM not (fTest test)
fTest (And t1 t2) = liftM2 (&&) (fTest t1) (fTest t2)
fTest (Or  t1 t2) = liftM2 (||) (fTest t1) (fTest t2)
fTest (Unique vs) = do
  vm <- get
  keyvals <- forM vs $ \v -> do
    let mp = Map.findWithDefault (error $ "Undefined variable " ++ v) v vm
    let (keys, vals) = unzip $ Map.toList mp
    return (keys, vals)
  let (k:ks, vals) = unzip keyvals
  let pairs = transpose vals
  when (any (/=k) ks) $ liftC $ fail "Different sets of indices"
  return $ unique pairs
fTest (InArray val var) = do
  vm <- get
  case Map.lookup var vm of
    Nothing -> liftC $ fail $ "Undefined variable " ++ var
    Just vals -> do
      v <- fExpr val
      return $ v `elem` Map.elems vals
fTest (CompOp cmp e1 e2) = do
  v1 <- fExpr e1 >>= toFloat -- note that Float here is also exact
  v2 <- fExpr e2 >>= toFloat
  let Just op = cmp `lookup` [ (CompGT, (>))
                             , (CompGE, (>=))
                             , (CompEQ, (==))
                             , (CompNE, (/=))
                             , (CompLT, (<))
                             , (CompLE, (<=)) ]
  return $ v1 `op` v2
fTest (Match e) = do
  s <- fExpr e >>= toString
  liftC $ match s
