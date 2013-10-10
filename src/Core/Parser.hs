module Parser 
where

import Language
import Data.Char
import Control.Arrow


type Token = (Int, String)
type Parser a = [Token] -> [(a, [Token])]

parse :: String -> CoreProgram
parse = syntax . clex


-- Lexer

twoCharOps = ["==", "˜=", ">=", "<=", "->"]

isNewline = (== '\n')
isWhiteSpace = flip elem " \t"
isIdChar c = isAlpha c || isDigit c || (c == '_')

clex :: String -> [Token]
clex = clex' 1

clex' :: Int -> String -> [Token]
-- skip whitespace
clex' n (c:cs) | isWhiteSpace c = clex' n cs

-- skip comments
clex' n ('|':'|':cs) = clex' n rest
    where
        rest = dropWhile (not . isNewline) cs 

-- skip newlines but increment line counter
clex' n (c:cs) | isNewline c = clex' (n+1) cs

-- extract two char ops
clex' n (c1:c2:cs) | op `elem` twoCharOps = (n, op) : clex' n cs
    where
        op = c1 : [c2]

-- extract numbers
clex' n (c:cs) | isDigit c = (n, num) : clex' n rest
    where
        num = c : takeWhile isDigit cs
        rest = dropWhile isDigit cs

-- extract identifiers
clex' n (c:cs) | isAlpha c = (n, var) : clex' n rest
    where
        var = c : takeWhile isIdChar cs
        rest = dropWhile isIdChar cs

clex' n (c:cs) = (n, [c]) : clex' n cs
clex' _ [] = []


-- parser combinators

keywords = ["let", "letrec", "case", "in", "of" , "Pack"]

-- parse if predicate returns true
pSat :: (String -> Bool) -> Parser String
pSat _ [] = []
pSat f (tok:toks) | f (snd tok) = [(snd tok, toks)]
                  | otherwise = []

-- parse String Literals
pLit :: String -> Parser String
pLit s = pSat (== s)

-- parse Variables
pVar :: Parser String
pVar = pSat f
    where
        f tok@(c:cs) = (not $ tok `elem` keywords) && isAlpha c && all isIdChar cs

-- parse Numbers
pNum :: Parser Int
pNum = pSat (all isDigit) `pApply` read 

-- parse Alternatives
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

-- parse Sequences
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks =
    [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks,
                               (v2, toks2) <- p2 toks1 ]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks =
   [ (combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks,
                                 (v2, toks2) <- p2 toks1,
                                 (v3, toks3) <- p3 toks2 ]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks =
   [ (combine v1 v2 v3 v4, toks4) | (v1, toks1) <- p1 toks,
                                    (v2, toks2) <- p2 toks1,
                                    (v3, toks3) <- p3 toks2,
                                    (v4, toks4) <- p4 toks3 ]

-- parse zero or more repetitions
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

-- parse one or more repetitions
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = take 1 . pThen (:) p (pZeroOrMore p)

-- parse empty repetition
pEmpty :: a -> Parser a
pEmpty v toks = [(v, toks)]

-- repetition with separator
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep pRep pSep = pThen (:) pRep pRest
    where
        -- use (flip const) for filtering the separator tokens
        pRest = pThen (flip const) pSep (pOneOrMoreWithSep pRep pSep) `pAlt` (pEmpty [])

-- apply func to parse result by mapping func on 
-- the fst part of the tuples (using Control.Arrow.first)
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f = map (first f) . p 

-- sequence two parsers and skip the result of the second one 
pLeft = pThen const

-- sequence two parsers and skip the result of the first one
pRight = pThen $ flip const


-- parsing Core

syntax :: [Token] -> CoreProgram
syntax = takeFirstParse . pProgram
    where
        takeFirstParse ((prog, []) : rest) = prog
        takeFirstParse (parse : rest) = takeFirstParse rest
        takeFirstParse _ = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen3 (,,) pVar (pZeroOrMore pVar) (pLit "=" `pRight` pExpr)

pExpr :: Parser CoreExpr
pExpr = pLet `pAlt` pCase `pAlt` pLam `pAlt` pAExpr

pLet = pThen3 ELet pLetType pDefns (pLit "in" `pRight` pExpr)

pLetType = (pLit "let" `pAlt` pLit "letrec") `pApply` isRec
    where 
        isRec "let" = nonRecursive
        isRec "letrec" = recursive

pDefns = pOneOrMoreWithSep pDefn (pLit ";")

pDefn = pThen (,) pVar (pLit "=" `pRight` pExpr)

pCase = pThen ECase (pLit "case" `pRight` pExpr) (pLit "of" `pRight` pAlts)
        
pAlts = pOneOrMoreWithSep pAlter (pLit ";")

pAlter = pThen3 (,,) pAltNum (pZeroOrMore pVar) (pLit "->" `pRight` pExpr)

pAltNum = (pLit "<") `pRight` (pNum `pLeft` pLit ">")

pLam = pThen ELam (pLit "\\" `pRight` pOneOrMore pVar) (pLit "." `pRight` pExpr)

pAExpr = pVarExpr `pAlt` pNumExpr `pAlt` pConstr `pAlt` pParanExpr

pVarExpr = pVar `pApply` EVar

pNumExpr = pNum `pApply` ENum

pConstr = pThen EConstr (pLit "Pack{" `pRight` pNum) (pNum `pLeft` pLit "}")

pParanExpr = pLit "(" `pRight` (pExpr `pLeft` pLit ")")

