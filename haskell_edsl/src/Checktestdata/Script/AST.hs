module Checktestdata.Script.AST (
  VarName,
  Var (..),
  Block,
  AST (..),
  Expr (..),
  BinOp (..),
  FloatOption (..),
  Test (..),
  CompOp (..),
  ) where

type VarName = String

data Var = Var VarName [Expr]
         deriving ( Show )

type Block = [AST]

data AST = CSpace
         | CNewline
         | CEOF
         | CInt Expr Expr (Maybe Var)
         | CFloat Expr Expr (Maybe Var) (Maybe FloatOption)
         | CString Expr
         | CRegex Expr (Maybe Var)
         | CRep (Maybe Var) Expr (Maybe AST) Block -- var, count, separator, body
         | CWhile (Maybe Var) Test (Maybe AST) Block
         | CAssert Test
         | CSet [(Var, Expr)]
         | CUnset [VarName]
         | CIf Test Block (Maybe Block)
         deriving (Show)

data FloatOption = Scientific
                 | Fixed
                 deriving (Show)

data Expr = EVar Var
          | ConstI Integer
          | ConstF Rational
          | ConstS String
          | Negate Expr
          | BinOp BinOp Expr Expr
          | StrLen Expr -- todo
          deriving (Show)

data BinOp = Plus
           | Minus
           | Times
           | Div
           | Modulo
           | Pow
           deriving (Show)

data Test = Not Test
          | CompOp CompOp Expr Expr
          | And Test Test
          | Or Test Test
          | Match Expr
          | Unique [VarName]
          | InArray Expr VarName
          | IsEOF
          deriving (Show)

data CompOp = CompGT | CompGE | CompEQ | CompNE | CompLT | CompLE
            deriving ( Show, Eq )
