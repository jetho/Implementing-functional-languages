module TI.Evaluator
where

import Core.Language
import Core.Parser
import Utils.Assoc
import Utils.Heap
import Data.List (mapAccumL)


type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
type TiHeap = Heap Node
type TiGlobals = ASSOC Name Addr
type TiStats = Int

data TiDump = DummyTiDump

data Node = NAp Addr Addr 
            | NSupercomb Name [Name] CoreExpr 
            | NNum Int 

extraPreludeDefs = []

runProg :: String -> String
runProg = showResults . eval . compile . parse

compile :: CoreProgram -> TiState
compile program = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
  where
    sc_defs = program ++ preludeDefs ++ extraPreludeDefs
    (initial_heap, globals) = buildInitialHeap sc_defs
    initial_stack = [address_of_main]
    address_of_main = aLookup globals "main" (error "main is not defined")

eval :: TiState -> [TiState]
eval = undefined

showResults :: [TiState] -> String
showResults = undefined

initialTiDump = DummyTiDump

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap = mapAccumL allocateSc hInitial

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NSupercomb name args body)

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps = (+ 1)

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps = id

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) = (stack, dump, heap, sc_defs, stats_fun stats)

