module Core.Language
where

type Name = String
type IsRec = Bool
type Alter a = (Int, [a], Expr a)
type ScDefn a = (Name, [a], Expr a)
type Program a = [ScDefn a]

type CoreExpr = Expr Name
type CoreAlt = Alter Name
type CoreScDefn = ScDefn Name
type CoreProgram = Program Name


-- AST
data Expr a = 
      EVar Name
    | ENum Int
    | EConstr Int Int
    | EAp (Expr a) (Expr a)
    | ELet IsRec [(a, Expr a)] (Expr a)
    | ECase (Expr a) [Alter a]
    | ELam [a] (Expr a)
    deriving (Show)


recursive = True
nonRecursive = False

bindersOf = map fst
rhssOf = map snd

isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr (EConstr _ _) = True 
isAtomicExpr _ = False


-- a small Prelude
preludeDefs :: CoreProgram
preludeDefs = 
    [ ("I", ["x"], EVar "x"),
      ("K", ["x", "y"], EVar "x"),
      ("K1", ["x", "y"], EVar "y"),
      ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))), 
      ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
      ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ] 

extraPreludeDefs :: CoreProgram
extraPreludeDefs =
    [ ("False", [], EConstr 1 0), 
      ("True", [], EConstr 2 0), 
      ("MkPair", [], EConstr 1 2), 
      ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K")),
      ("snd", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1")),
      ("Nil", [], EConstr 1 0), 
      ("Cons", [], EConstr 2 2), 
      ("head", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "abort")) (EVar "K")),
      ("tail", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "abort")) (EVar "K1")),
      ("length", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (ENum 0)) (EVar "length'")),
      ("length'", ["x","xs"], EAp (EAp (EVar "+") (ENum 1)) (EAp (EVar "length") (EVar "xs"))),
      ("and", ["x","y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) (EVar "False")),
      ("or", ["x","y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "x")) (EVar "y")),
      ("not", ["x"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "False")) (EVar "True")),
      ("xor",["x","y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EAp (EVar "not") (EVar "y"))) (EVar "y")) ]      

