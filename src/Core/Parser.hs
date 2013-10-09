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
pNum = flip pApply read $ pSat (all isDigit)

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
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr

pExpr :: Parser CoreExpr
pExpr = pThen4 mkLet (pLit "let" `pAlt` pLit "letrec") pDefns (pLit "in") pExpr 
        `pAlt` pThen4 mkCase (pLit "case") pExpr (pLit "of") pAlts 
        `pAlt` pThen4 mkLambda (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr 
        `pAlt` pAExpr

pDefns = pOneOrMoreWithSep pDefn (pLit ";")

pDefn = pThen3 mkDefn pVar (pLit "=") pExpr

pAlts = pOneOrMoreWithSep pAlter (pLit ";")

pAlter = pThen4 mkAlt pAltNum (pZeroOrMore pVar) (pLit "->") pExpr

pAltNum = pThen3 mkAltNum (pLit "<") pNum (pLit ">")

pAExpr = pVarExpr `pAlt` pNumExpr `pAlt` pConstr `pAlt` pParanExpr

pVarExpr = pApply pVar EVar

pNumExpr = pApply pNum ENum

pConstr = pThen4 mkConstr (pLit "Pack{") pNum pNum (pLit "}")

pParanExpr = pThen3 extractExpr (pLit "(") pExpr (pLit ")")
    where
        extractExpr _ e _ = e


-- AST constructor functions

mkSc name vars eq expr = (name, vars, expr)

mkLet letType defns _ expr = ELet isRec defns expr
    where
        isRec = if letType == "let" then nonRecursive else recursive

mkCase _ expr _ alts = ECase expr alts

mkLambda _ vars _ expr = ELam vars expr

mkDefn var _ expr = (var, expr)

mkAlt tag vars _ expr = (tag, vars, expr)

mkAltNum _ n _ = n

mkConstr _ n1 n2 _ = EConstr n1 n2

