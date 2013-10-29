module TI.Evaluator
where

import Core.Language
import Core.Parser
import Core.PrettyPrinter
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

initialTiDump = DummyTiDump

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap = mapAccumL allocateSc hInitial

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NSupercomb name args body)


eval :: TiState -> [TiState]
eval state = state : rest_states
    where
        rest_states | tiFinal state = []
                    | otherwise = eval next_state
        next_state = doAdmin (step state)

step state = dispatch (hLookup heap (head stack))
    where
        (stack, dump, heap, globals, stats) = state
        dispatch (NNum n) = numStep state n
        dispatch (NAp a1 a2) = apStep state a1 a2
        dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 = (a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body
    | length arg_names < length stack = (new_stack, dump, new_heap, globals, stats)
    | otherwise = error $ "Not enough args for supercombinator " ++ sc_name
    where
        new_stack = result_addr : (drop (length arg_names+1) stack)
        (new_heap, result_addr) = instantiate body heap env
        env = arg_bindings ++ globals
        arg_bindings = zip arg_names (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack) = map get_arg stack
    where
        get_arg addr = arg where (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> [(Name, Addr)] -> (TiHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap $ NNum n
instantiate (EAp e1 e2) heap env = hAlloc heap'' $ NAp a1 a2
    where
        (heap', a1) = instantiate e1 heap env
        (heap'', a2) = instantiate e2 heap' env
instantiate (EVar v) heap env = (heap, aLookup env v $ error msg)
    where
        msg = "Undefined name " ++ show v
instantiate (EConstr tag arity) heap env =
    instantiateConstr tag arity heap env
instantiate (ELet isRec defs body) heap env =
    instantiateLet isRec defs body heap env
instantiate (ECase e alts) heap env = error "Can’t instantiate case exprs"

instantiateDefs :: [(Name, CoreExpr)] -> TiHeap -> ASSOC Name Addr -> (TiHeap, ASSOC Name Addr)
instantiateDefs xs heap env = foldr instantiateDef (heap, []) xs
    where instantiateDef (name, expr) (heap', bindings) =
              let (heap'', addr) = instantiate expr heap' env
              in (heap'', (name, addr):bindings)

instantiateLet :: IsRec -> [(Name, CoreExpr)] -> CoreExpr -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instantiateLet False defs body heap env = instantiate body heap' (bindings ++ env)
    where
        (heap', bindings) = instantiateDefs defs heap env

instantiateLet True defs body heap env = instantiate body heap' env'
    where
        (heap', bindings) = instantiateDefs defs heap env'
        env' = bindings ++ env

instantiateConstr = undefined

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats) = isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node = False

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats) = iConcat [ showStack heap stack, showHeap heap stack, iNewline ]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack =
    iConcat [ iStr "Stk [",
              iIndent (iInterleave iNewline (map show_stack_item stack)),
              iStr " ]" ]
    where
        show_stack_item addr =
            iConcat [ showFWAddr addr, iStr ": ",
                      showStkNode heap (hLookup heap addr) ]

showHeap :: TiHeap -> TiStack -> Iseq
showHeap heap stack
   = iConcat [ iNewline, iStr " Heap [",
               iIndent (iInterleave iNewline (map show_heap_item $ hAddresses heap)),
               iStr " ], Length: ",
               iStr $ show $ hSize heap ]
   where
   show_heap_item addr
      = iConcat [ showFWAddr addr, iStr ": ",
                  showStkNode heap (hLookup heap addr)
                ]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr) =
    iConcat [ iStr "NAp ", showFWAddr fun_addr,
              iStr " ", showFWAddr arg_addr, iStr " (",
              showNode (hLookup heap arg_addr), iStr ")" ]
showStkNode heap node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) = iConcat [ iStr "NAp ", iStr (showAddr a1), iStr " ", iStr (showAddr a2) ]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = (iStr "NNum ") `iAppend` (iNum n)
  
showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (space (4 - length str) ++ str)
    where
        str = show addr

showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats) =
    iConcat [ iNewline, iNewline, iStr "Total number of steps = ",
              iNum (tiStatGetSteps stats) ]

showResults :: [TiState] -> String
showResults states = iDisplay (iConcat [ iLayn (map showState states), showStats (last states) ])


tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps = (+ 1)

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps = id

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) = (stack, dump, heap, sc_defs, stats_fun stats)

