module Core.PrettyPrinter
where

import Core.Language
import Data.List

-- data type for the pretty printer
data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline 
          deriving Show

-- constructor functions
iNil = INil

-- take care of newlines when wrapping a string
iStr = iInterleave iNewline . map IStr . lines

iNum = iStr . show

iAppend INil expr = expr
iAppend expr INil = expr
iAppend seq1 seq2 = IAppend seq1 seq2

iIndent seq = IIndent seq

iNewline = INewline

iConcat = foldr iAppend iNil

iFWNum width n = iStr (space (width - length digits) ++ digits)
    where 
        digits = show n

iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
    where 
        lay_item (n,seq) = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline]

iInterleave sep [] = iNil
iInterleave sep (x:xs) = x `iAppend` (foldr combine iNil xs)
    where 
        combine = iAppend . iAppend sep

iEnclose s INil = s
iEnclose s content = s `iAppend` content `iAppend` s


-- pretty printing the AST  

pprint :: CoreProgram -> String
pprint = iDisplay . pprProgram

pprProgram  = iInterleave iNewline . map superCombinator
    where 
        superCombinator (name, args, expr) = iConcat [ (iStr name), pprArgs args, iStr "= ", pprExpr expr, iStr ";" ]
        pprArgs = iEnclose (iStr " ") . iInterleave (iStr " ") . pprVars

pprExpr (ENum n) = iNum n
pprExpr (EVar v) = iStr v
pprExpr (EConstr x y) = iConcat [ iStr "Pack {", iNum x, iStr ", ", iNum y, iStr "}" ] 
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr) =
    iConcat [ iStr keyword, iIndent (pprDefns defns), iNewline, 
              iStr "in ", iIndent (iNewline `iAppend` pprExpr expr) ]
    where 
        keyword | not isrec = "let"
                | isrec = "letrec"

pprExpr (ECase expr alts) =
    iConcat [ iStr "case ", pprExpr expr, iStr " of", iIndent (pprAlts alts) ]
    where 
        pprAlts = iAppend iNewline . iInterleave iNewline . map pprAlt

pprExpr (ELam [] expr) = pprExpr expr
pprExpr (ELam vars expr) = 
    iConcat [ iStr "( \\ ", iInterleave (iStr ", ") (pprVars vars), iStr " . ", pprExpr expr , iStr " )" ]


-- helper functions for transforming the AST

pprAlt (n, vars, expr) = 
    iConcat [ iStr "<", iStr $ show n, iStr ">" , 
              iEnclose (iStr " ") $ iInterleave (iStr " ") (pprVars vars), 
              iStr "-> ", iIndent (pprExpr expr) ]

pprVars = map iStr

pprDefns = iAppend iNewline . iInterleave sep . map pprDefn
    where 
        sep = iConcat [ iStr ";", iNewline ]

pprDefn (name, expr) =
    iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

pprAExpr e | isAtomicExpr e = pprExpr e
pprAExpr e | otherwise = iStr "(" `iAppend` (pprExpr e) `iAppend` iStr ")"


-- flattening the pretty printing structure

iDisplay seq =  flatten 0 [(seq, 0)]

flatten :: Int -> [(Iseq, Int)] -> String
flatten _ [] = ""

flatten col ((INewline, indent) : seqs) = 
    '\n' : space indent ++ (flatten indent seqs)

flatten col ((IIndent seq, indent) : seqs) = 
    flatten col ((seq, indentation) : seqs)
    where 
        indentation = indent + 2

flatten col (((IAppend seq1 seq2), indent) : seqs) =
    flatten col ((seq1, indent) : (seq2, indent) : seqs)

flatten col ((IStr str, indent) : seqs) = str ++ (flatten col seqs)

flatten col ((INil, indent) : seqs) = flatten col seqs

space = flip replicate ' ' 

