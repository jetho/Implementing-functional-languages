module TI.Evaluator
where

import Core.Language
import Core.Parser
import Core.PrettyPrinter
import Utils.Assoc
import Utils.Heap
import Data.List (mapAccumL, sort)


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

data Primitive = Neg | Add | Sub | Mul | Div


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

initialTiDump = []

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
               ("/", Div) ]

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
        dispatch (NInd a) = indStep state a
        dispatch (NSupercomb sc args body) = scStep state sc args body
        dispatch (NPrim name prim) = primStep prim state

numStep :: TiState -> Int -> TiState
numStep ([_], stack:dump, heap, globals, stats) _ = (stack, dump, heap, globals, stats)
numStep state n = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 = 
    case hLookup heap a2 of
        (NInd a3) -> (stack, dump, hUpdate heap (head stack) (NAp a1 a3), globals, stats)
        otherwise -> (a1 : stack, dump, heap, globals, stats)

indStep :: TiState -> Addr -> TiState
indStep (a:as, dump, heap, globals, stats) a1 = (a1 : as, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body  
    | length arg_names < length stack = (stack', dump, heap', globals, stats)
    | otherwise = error $ "Not enough args for supercombinator " ++ sc_name
    where
        heap' = instantiateAndUpdate body (stack !! length arg_names) heap env 
        env = arg_bindings ++ globals
        arg_bindings = zip arg_names (getArgs heap stack)
        stack' = drop (length arg_names) stack

primStep :: Primitive -> TiState -> TiState
primStep Neg = primNeg 
primStep Add = primArith (+)
primStep Sub = primArith (-)
primStep Mul = primArith (*)
primStep Div = primArith div
primStep _ = error "Unknown Primitive"

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
primDyadic (stack@[a,a1,a2], dump, heap, globals, stats) op
    | bothEvaluated = ([b'], dump, hUpdate heap' a2 n', globals, stats)
    | otherwise = ([b2], [b1]:[a2]:dump, heap, globals, stats)
    where 
        [b1, b2] = getArgs heap stack
        n1 = hLookup heap b1
        n2 = hLookup heap b2
        bothEvaluated = isDataNode n1 && isDataNode n2
        n' = n1 `op` n2
        (heap', b') = hAlloc heap n'

primNeg :: TiState -> TiState
primNeg (stack, dump, heap, globals, stats)
    | length stack /= 2 = error $ "Type error: wrong number of arguments"
    | isDataNode node = (stack', dump, hUpdate heap result_addr (NNum (-n)), globals, stats)
    | otherwise = ([arg_addr], (stack' : dump), heap, globals, stats)
    where 
        arg_addr = head $ getArgs heap stack
        node = hLookup heap arg_addr
        (NNum n) = node
        stack' = tail stack
        result_addr = head stack'

-- arithmetic
primArith :: (Int -> Int -> Int) -> TiState -> TiState
primArith op state =
   primDyadic state (arithFun op)
   where
   arithFun op (NNum a) (NNum b) = (NNum (a `op` b))
   arithFun op _ _ = error "Wrong data nodes"

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc:stack) = map get_arg stack
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

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> ASSOC Name Addr -> TiHeap
instantiateAndUpdate (EVar v) upd_addr heap env = hUpdate heap upd_addr $ NInd $ aLookup env v $ error msg
    where 
        msg = "Undefined name " ++ show v
instantiateAndUpdate (ENum n) upd_addr heap env = hUpdate heap upd_addr (NNum n)
instantiateAndUpdate (EAp e1 e2) upd_addr heap env = hUpdate heap2 upd_addr (NAp a1 a2)
    where 
        (heap1, a1) = instantiate e1 heap env
        (heap2, a2) = instantiate e2 heap1 env
instantiateAndUpdate letExpr@(ELet isrec defs body) upd_addr heap env = hUpdate heap' upd_addr (NInd addr)
    where 
        (heap', addr) = instantiate letExpr heap env
instantiateAndUpdate (ECase guard alts) upd_addr heap env = error "Can’t instantiate case exprs"

instantiateDefs :: [(Name, CoreExpr)] -> TiHeap -> ASSOC Name Addr -> (TiHeap, ASSOC Name Addr)
instantiateDefs xs heap env = foldr instantiateDef (heap, []) xs
    where 
        instantiateDef (name, expr) (heap', bindings) =
            let (heap'', addr) = instantiate expr heap' env
            in (heap'', (name, addr) : bindings)

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
tiFinal ([sole_addr], [], heap, globals, stats) = isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node = False

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats) =
    iConcat [ showStack heap stack, showHeap heap stack, showDump heap stack dump, iNewline ]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack = 
    iConcat [ iStr "Stk [", iIndent (iInterleave iNewline (map show_stack_item stack)), iStr " ]" ]
    where
        show_stack_item addr = iConcat [ showFWAddr addr, iStr ": ", showStkNode heap (hLookup heap addr) ]

showHeap :: TiHeap -> TiStack -> Iseq
showHeap heap stack = 
    iConcat [ iNewline, iStr " Heap [",
              iIndent (iInterleave iNewline (map show_heap_item $ reverseSort (hAddresses heap))),
              iStr " ], Length: ", iStr $ show $ hSize heap ]
    where
        reverseSort = reverse . sort
        show_heap_item addr = iConcat [ showFWAddr addr, iStr ": ", showStkNode heap (hLookup heap addr) ]

showDump :: TiHeap -> TiStack -> TiDump -> Iseq
showDump heap stack dump =
    iConcat [ iNewline, iStr " Dump [",
              iIndent ( iInterleave iNewline (map show_dump_item $ dump)),
              iStr " ]"]
    where
        show_dump_item stk = showStack heap stk

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr) = 
    iConcat [ iStr "NAp ", showFWAddr fun_addr,
              iStr " ", showFWAddr arg_addr, iStr " (",
              showNode (hLookup heap arg_addr), iStr ")" ]
showStkNode heap node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) = iConcat [ iStr "NAp ", iStr (showAddr a1), iStr " ", iStr (showAddr a2) ]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = iStr "NNum " `iAppend` iNum n
showNode (NInd a) = iStr "NInd " `iAppend` iStr (showAddr a)
showNode (NPrim name prim) = iStr ("Primitive: " ++ name) 

showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr $ space (4 - length str) ++ str
    where 
        str = show addr

showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats) = 
    iConcat [ iNewline, iNewline, iStr "Total number of steps = ", iNum (tiStatGetSteps stats) ]

showResults :: [TiState] -> String
showResults states = iDisplay $ iConcat [ iLayn (map showState states), showStats (last states) ]


tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps = (+ 1)

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps = id

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) = (stack, dump, heap, sc_defs, stats_fun stats)

