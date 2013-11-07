module TI.Types
where

import Core.Language
import Utils.Assoc
import Utils.Heap


type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
type TiHeap = Heap Node
type TiGlobals = ASSOC Name Addr
type TiStats = Int
type TiDump = [TiStack]


data Node = NAp Addr Addr 
          | NSupercomb Name [Name] CoreExpr 
          | NNum Int 
          | NInd Addr
          | NPrim Name Primitive
          | NData Int [Addr]

		  
data Primitive = Neg 
               | Add 
               | Sub 
               | Mul 
               | Div 
               | PrimConstr Int Int
               | If 
               | Greater 
               | GreaterEq 
               | Less 
               | LessEq 
               | Eq 
               | NotEq
               deriving (Show)


tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps = (+ 1)

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps = id
