module PrettyPrinter
where

import Data.List
import Language


-- data type for the pretty printer
data Iseq = INil
          | IStr String
          | IAppend Iseq Iseq
          | IIndent Iseq
          | INewline 
          deriving Show


-- constructor functions
--
iNil = INil

-- take care of newlines when wrapping a string
iStr = iInterleave iNewline . map IStr . lines

iAppend INil expr = expr
iAppend expr INil = expr
iAppend seq1 seq2	= IAppend seq1 seq2

iIndent seq = IIndent seq

iNewline = INewline

iConcat = foldl' iAppend iNil

iInterleave sep [] = iNil
iInterleave sep (x:xs) = x `iAppend` (foldr combine iNil xs)
    where 
    combine = iAppend . iAppend sep

iEnclose s INil = s
iEnclose s content = s `iAppend` content `iAppend` s


-- pretty print the AST  
--
pprint :: CoreProgram -> String
pprint = iDisplay . pprProgram


pprProgram  = iInterleave iNewline . map superCombinator
    where
    superCombinator (name, args, expr) =
        iConcat [ (iStr name), pprArgs args, iStr "= ", pprExpr expr, iStr ";"]
    pprArgs = iEnclose (iStr " ") . iInterleave (iStr " ") . pprVars


pprExpr (ENum n) = iStr $ show n
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr) =
    iConcat [ iStr keyword, iNewline, iIndent (pprDefns defns), iNewline,
              iStr "in ", pprExpr expr ]
    where
    keyword | not isrec = "let"
            | isrec = "letrec"

pprExpr (ECase expr alts) =
    iConcat [iStr "case " , pprExpr expr, iStr " of", iNewline, pprAlts alts]
    where 
    pprAlts = iInterleave iNewline . map (iIndent . pprAlt)
    

pprExpr (ELam [] expr) = pprExpr expr
pprExpr (ELam vars expr) = 
    iConcat [iStr "( \\ ", iInterleave (iStr ", ") (pprVars vars), iStr " . ", pprExpr expr , iStr " )"]



-- helper functions for converting the AST
--
pprAlt (n, vars, expr) = 
        iIndent ( iConcat [ iStr "<", iStr $ show n, iStr ">" , 
            iEnclose (iStr " ") $ iInterleave (iStr " ") (pprVars vars), 
            iStr "-> ", pprExpr expr] )

pprVars = map iStr

pprDefns = iInterleave sep . map (iIndent . pprDefn)
    where
    sep = iConcat [ iStr ";", iNewline ]

pprDefn (name, expr) =
    iConcat [ iStr name, iStr " = ", pprExpr expr ]

pprAExpr e | isAtomicExpr e = pprExpr e
pprAExpr e | otherwise = iStr "(" `iAppend` (pprExpr e) `iAppend` iStr ")"



-- flattening the pretty printing structure
--
iDisplay seq =  flatten 0 [(seq, 0)]


flatten :: Int -> [(Iseq, Int)] -> String
flatten _ [] = ""

flatten col ((INewline, indent) : seqs) = '\n' : (flatten indent seqs)

flatten col ((IIndent seq, indent) : seqs) = 
    space col ++ flatten (col+2) ((seq, indent) : seqs )

flatten col (((IAppend seq1 seq2), indent) : seqs) =
    (flatten col [(seq1, indent)]) ++ (flatten col [(seq2, indent)]) ++ (flatten col seqs)

flatten col ((IStr str, indent) : seqs) = str ++ (flatten col seqs)

flatten col ((INil, indent) : seqs) = (flatten col seqs)

space n = take n $ repeat ' ' 

