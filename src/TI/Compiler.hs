module TI.Compiler
where

import Core.Language
import Utils.Assoc
import Utils.Heap
import Data.List (mapAccumL)
import TI.Types


compile :: CoreProgram -> TiState
compile program = (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
    where
        sc_defs = program ++ preludeDefs ++ extraPreludeDefs
        (initial_heap, globals) = buildInitialHeap sc_defs
        initial_stack = [address_of_main]
        address_of_main = aLookup globals "main" (error "main is not defined")

initialTiDump :: TiDump
initialTiDump = []

tiStatInitial :: TiStats
tiStatInitial = 0

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = (heap2, sc_addrs ++ prim_addrs)
    where
        (heap1, sc_addrs) = mapAccumL allocateSc hInitial sc_defs
        (heap2, prim_addrs) = mapAccumL allocatePrim heap1 primitives

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim) = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NPrim name prim)
	
primitives :: ASSOC Name Primitive
primitives = [ ("negate", Neg),
               ("+", Add),
               ("-", Sub),
               ("*", Mul),
               ("/", Div),
               ("if", If),
               ("<", Less),
               ("<=", LessEq),
               (">", Greater),
               (">=", GreaterEq),
               ("==", Eq),
               ("!=", NotEq),
               ("casePair", CasePair),
               ("caseList", CaseList),
               ("abort", Abort) ]
