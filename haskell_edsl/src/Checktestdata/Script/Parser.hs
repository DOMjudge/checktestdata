{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Checktestdata.Script.Parser (
  parseScript
  ) where

import Checktestdata.Script.AST

--import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Language
--import Text.ParserCombinators.Parsec.Expr
--import qualified Text.ParserCombinators.Parsec.Token as Token

import Data.Char
import Data.Ratio

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

parseScript :: FilePath -> IO Block
parseScript fp = do
  contents <- readFile fp
  case execParser (pSpaces *> pBlock) (dropComments contents) of
    (r, [])  -> return r
    (r, err) -> mapM_ print err >> return r

dropComments :: String -> String
dropComments = unlines . map (takeWhile (/='#')) . lines

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

pBlock :: Parser Block
pBlock = pList pAST

pAST :: Parser AST
pAST = CSpace    <$  pSymbol "SPACE"
   <<|> CNewline <$  pSymbol "NEWLINE"
   <<|> CEOF     <$  pSymbol "EOF"
   <<|> CInt     <$  pSymbol "INT"
                 <*  pLParen
                 <*> pExpr
                 <*  pComma
                 <*> pExpr
                 <*> (Just <$ pComma <*> identifier <<|> pure Nothing)
                 <*  pRParen
   <<|> CFloat   <$  pSymbol "FLOAT"
                 <*  pLParen
                 <*> pExpr
                 <*  pComma
                 <*> pExpr
                 <*> (Just <$ pComma <*> identifier <<|> pure Nothing)
                 <*> (Just <$ pComma <*> pFloatOption <<|> pure Nothing)
                 <*  pRParen
   <<|> CRep     <$  pSymbol "REPI"
                 <*  pLParen
                 <*> (Just <$> identifier <* pComma)
                 <*> pExpr
                 <*> (Just <$ pComma <*> pAST <<|> pure Nothing)
                 <*  pRParen
                 <*> pBlock
                 <*  pSymbol "END"
   <<|> CRep     <$  pSymbol "REP"
                 <*  pLParen
                 <*> pure Nothing
                 <*> pExpr
                 <*> (Just <$ pComma <*> pAST <<|> pure Nothing)
                 <*  pRParen
                 <*> pBlock
                 <*  pSymbol "END"
   <<|> CWhile   <$  pSymbol "WHILEI"
                 <*  pLParen
                 <*> (Just <$> identifier <* pComma)
                 <*> pTest
                 <*> (Just <$ pComma <*> pAST <<|> pure Nothing)
                 <*  pRParen
                 <*> pBlock
                 <*  pSymbol "END"
   <<|> CWhile   <$  pSymbol "WHILE"
                 <*  pLParen
                 <*> pure Nothing
                 <*> pTest
                 <*> (Just <$ pComma <*> pAST <<|> pure Nothing)
                 <*  pRParen
                 <*> pBlock
                 <*  pSymbol "END"
   <<|> CIf      <$  pSymbol "IF"
                 <*  pLParen
                 <*> pTest
                 <*  pRParen
                 <*> pBlock
                 <*> (Just <$ pSymbol "ELSE" <*> pBlock <<|> pure Nothing)
                 <*  pSymbol "END"
   <<|> CAssert  <$  pSymbol "ASSERT"
                 <*  pLParen
                 <*> pTest
                 <*  pRParen
   <<|> CString  <$  pSymbol "STRING"
                 <*  pLParen
                 <*> pExpr
                 <*  pRParen
   <<|> CSet     <$  pSymbol "SET"
                 <*  pLParen
                 <*> pListSep pComma
                     (
                       (,) <$> identifier
                           <*  pSym '='
                           <*> pExpr
                     )
                 <*  pRParen

pFloatOption :: Parser FloatOption
pFloatOption = Scientific <$ pSymbol "SCIENTIFIC"
          <<|> Fixed <$ pSymbol "FIXED"

pExpr :: Parser Expr
pExpr = foldr pChainl pExprBase (map same_prio operators) where
  same_prio ops = msum [ BinOp op <$ pSym c | (c, op) <- ops]
  pExprBase :: Parser Expr
  pExprBase = pParens pExpr
    <<|> Negate <$  pSym '-'
                <*> pExpr
    <<|> EVar   <$> identifier
    <<|> ConstS <$> pString
    <<|> lexeme pNumber

  operators :: [[ (Char, BinOp) ]]
  operators = [ [('+', Plus), ('-', Minus)]
              , [('*' , Times), ('/', Div), ('%', Modulo)]
              , [('^', Pow)]
              ]

-- | Parses integers and floating point numbers. Only positive numbers are parsed
--   here, the Negate constructor is used for negative numbers, this is to avoid
--   parsing ambiguity.
pNumber :: Parser Expr
pNumber = mkNum
  <$> pNatural
  <*> (Just <$ pSym '.'     <*> pMunch isDigit <<|> pure Nothing)
                                -- exponent can be negative
  <*> (Just <$ pAnySym "eE" <*> pInteger       <<|> pure Nothing) where
  
  mkNum :: Integer -> Maybe String -> Maybe Integer -> Expr
  mkNum i Nothing Nothing = ConstI i
  mkNum i mbF mbE = ConstF $ (fromInteger i + fpart) * epart where
    fpart = case mbF of
      Nothing -> 0
      Just f  -> (read f) % (10 ^ length f)
    epart = case mbE of
      Nothing -> 1
      Just e  -> 10 ^^ e

-- | Parse a literal string
pString :: Parser String
pString = lexeme $ pSym '"' *> pList pChar <*  pSym '"' where
  pChar :: Parser Char -- todo: \[0-7]{1,3} denotes an octal escape for a character
  pChar = '\n' <$ pToken "\\n"
     <<|> '\t' <$ pToken "\\t"
     <<|> '\r' <$ pToken "\\r"
     <<|> '\b' <$ pToken "\\b"
     <<|> '"'  <$ pToken "\\\""
     <<|> '\\' <$ pToken "\\\\"
     <<|> pToken "\\\n" *> pSatisfy (/='"') (Insertion "x" 'x' 5)
     <<|> pSatisfy (/='"') (Insertion "x" 'x' 5)

-- | Parse boolean expressions
pTest :: Parser Test
pTest = pChainl bOps pTestBase where
  bOps = And <$ pSymbol "&&"
    <<|> Or  <$ pSymbol "||"
  pTestBase = pParens pTest
         <<|> Not     <$  pSym '!'
                      <*> pTest
         <<|> IsEOF   <$  pSymbol "ISEOF"
         <<|> Match   <$  pSymbol "MATCH"
                      <*  pLParen
                      <*> pExpr
                      <*  pRParen
         <<|> Unique  <$  pSymbol "UNIQUE"
                      <*  pLParen
                      <*> pListSep pComma identifier
                      <*  pRParen
         <<|> InArray <$  pSymbol "INARRAY"
                      <*  pLParen
                      <*> pExpr
                      <*  pComma
                      <*> identifier
                      <*  pRParen
         <<|> compOps
  compOps = msum [ CompOp op <$> pExpr <* pSymbol c <*> pExpr
                 | (c, op) <- [ ("<",  CompLT)
                              , ("<=", CompLE)
                              , ("==", CompEQ)
                              , ("!=", CompNE)
                              , (">",  CompGT)
                              , (">=", CompGE)
                              ] ]
  

--------------------------------------------------------------------------------
-- Lexing
--------------------------------------------------------------------------------

identifier :: Parser String
identifier = lexeme $ (:) <$> pLetter <*> pMunch isAlphaNum
