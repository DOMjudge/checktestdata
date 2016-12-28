module Checktestdata.Script.AST (
  Var,
  Block,
  AST (..),
  Expr (..),
  BinOp (..),
  FloatOption (..),
  Test (..),
  CompOp (..),
  ) where
-- data Var = Var String [Expr]

type Var = String

type Block = [AST]

data AST = CSpace
         | CNewline
         | CEOF
         | CInt Expr Expr (Maybe Var)
         | CFloat Expr Expr (Maybe Var) (Maybe FloatOption)
         | CString Expr
         | CRep (Maybe Var) Expr (Maybe AST) Block -- var, count, separator, body
         | CWhile (Maybe Var) Test (Maybe AST) Block
         | CAssert Test
         | CSet [(Var, Expr)]
         | CUnset [Var]
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
          | Unique [Var]
          | InArray Expr Var
          | IsEOF
          deriving (Show)

data CompOp = CompGT | CompGE | CompEQ | CompNE | CompLT | CompLE
            deriving ( Show, Eq )
