{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Checktestdata.Script.Parser (
  parseScript
  ) where

import Checktestdata.Script.AST

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
  let b = runParser fp (pSpaces *> pBlock) (dropComments contents)
  b `seq` return b

-- | Remove all comments from the text. This is less trivial than it seems
--   as # may also be inside a string literal.
dropComments :: String -> String
dropComments = f False False where
  f :: Bool -> Bool -> String -> String -- inComment, inString
  f _     _     ""        = ""
  f True  False ('\n':xs) = '\n' : f False False xs
  f True  False (   _:xs) =        f True  False xs
  f False True  ('\\':x:xs) = '\\' : x : f False True xs
  f False True  ('"' :xs) = '"'  : f False False xs
  f False True  (   x:xs) = x    : f False True  xs
  f False False ('"' :xs) = '"'  : f False True  xs
  f False False ('#' :xs) =        f True  False xs
  f False False (   x:xs) = x    : f False False xs
  f _     _     _         = error "dropComments: invariant failed"
  

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
                 <*> (Just <$ pComma <*> pVar <<|> pure Nothing)
                 <*  pRParen
   <<|> CFloat   <$  pSymbol "FLOAT"
                 <*  pLParen
                 <*> pExpr
                 <*  pComma
                 <*> pExpr
                 <*> (Just <$ pComma <*> pVar <<|> pure Nothing)
                 <*> (Just <$ pComma <*> pFloatOption <<|> pure Nothing)
                 <*  pRParen
   <<|> CRep     <$  pSymbol "REPI"
                 <*  pLParen
                 <*> (Just <$> pVar <* pComma)
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
                 <*> (Just <$> pVar <* pComma)
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
   <<|> CRegex   <$  pSymbol "REGEX"
                 <*  pLParen
                 <*> pExpr
                 <*> (Just <$ pComma <*> pVar <<|> pure Nothing)
                 <*  pRParen
   <<|> CString  <$  pSymbol "STRING"
                 <*  pLParen
                 <*> pExpr
                 <*  pRParen
   <<|> CSet     <$  pSymbol "SET"
                 <*  pLParen
                 <*> pListSep pComma
                     (
                       (,) <$> pVar
                           <*  lexeme (pSym '=')
                           <*> pExpr
                     )
                 <*  pRParen
   <<|> CUnset   <$  pSymbol "UNSET"
                 <*  pLParen
                 <*> pListSep pComma identifier
                 <*  pRParen

pFloatOption :: Parser FloatOption
pFloatOption = Scientific <$ pSymbol "SCIENTIFIC"
          <<|> Fixed <$ pSymbol "FIXED"

pExpr :: Parser Expr
pExpr = foldr pChainl pExprBase (map same_prio operators) where
  same_prio ops = msum [ BinOp op <$ lexeme (pSym c) | (c, op) <- ops]
  pExprBase :: Parser Expr
  pExprBase = pParens pExpr
    <<|> Negate <$  lexeme (pSym '-')
                <*> pExpr
    <<|> StrLen <$  pSymbol "STRLEN"
                <*  pLParen
                <*> pExpr
                <*  pRParen
    <<|> EVar   <$> pVar
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
      Just "" -> 0
      Just f  -> (read f) % (10 ^ length f)
    epart = case mbE of
      Nothing -> 1
      Just e  -> 10 ^^ e

-- | Parse a literal string
pString :: Parser String
pString = lexeme $ pSym '"' *> pList pChar <* pSym '"' where
  pChar :: Parser Char
  pChar = pSym '\\' *> ( pOctal
                    <<|> '\n' <$ pSym 'n'
                    <<|> '\t' <$ pSym 't'
                    <<|> '\r' <$ pSym 'r'
                    <<|> '\b' <$ pSym 'b'
                    <<|> '\\' <$ pSym '\\'
                    <<|> '"'  <$ pSym '"'
                    <<|> pSym '\n' *> pChar
                    <<|> pure '\\')
    <<|> pSatisfy (/='"') (Insertion "x" 'x' 5)
  pOctal :: Parser Char
  pOctal = toOct <$> octDig
                 <*> (Just <$> octDig <<|> pure Nothing)
                 <*> (Just <$> octDig <<|> pure Nothing)
  toOct :: Char -> Maybe Char -> Maybe Char -> Char
  toOct d1 (Just d2) (Just d3) = chr $ 64 * toNum d1 + 8 * toNum d2 + toNum d3
  toOct d1 (Just d2) Nothing   = chr $ 8 * toNum d1 + toNum d2
  toOct d1 Nothing   Nothing   = chr $ toNum d1
  toOct _  _         _         = error $ "toOct: invariant failed"
  toNum :: Char -> Int
  toNum c = ord c - ord '0'
  octDig :: Parser Char
  octDig = pSatisfy (\c -> '0' <= c && c <= '7') (Insertion "0" '0' 5)  

-- | Parse boolean expressions
pTest :: Parser Test
pTest = pChainl bOps pTestBase where
  bOps = And <$ pSymbol "&&"
    <<|> Or  <$ pSymbol "||"
  pTestBase = pParens pTest
         <<|> Not     <$  lexeme (pSym '!')
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
  

-- | Parse a variable (possibly with array indices)
pVar :: Parser Var
pVar = Var <$> identifier
           <*> ( pLBracket *> pListSep pComma pExpr <* pRBracket <<|> pure [])

--------------------------------------------------------------------------------
-- Lexing
--------------------------------------------------------------------------------

identifier :: Parser String
identifier = lexeme $ (:) <$> pLower <*> pMunch (\c -> isLower c || isDigit c)
