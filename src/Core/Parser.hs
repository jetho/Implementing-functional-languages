module Parser 
where

import Language
import Data.Char


type Token = (Int, String)
type Parser a = [Token] -> [(a, [Token])]

parse :: String -> CoreProgram
parse = syntax . clex


-- Lexer

twoCharOps = ["==", "˜=", ">=", "<=", "->"]
isNewline = (== '\n')
isWhiteSpace = flip elem " \t"

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
        isIdChar c = isAlpha c || isDigit c || (c == '_')

clex' _ [] = []



-- parsing functions

pLit :: String -> Parser String
pLit s (tok:toks) | snd tok == s = [(s, toks)]
                  | otherwise = []
pLit _ [] = []

syntax = undefined
