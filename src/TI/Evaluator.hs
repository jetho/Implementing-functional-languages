module TI.Evaluator
where

import Core.Language
import Utils.Assoc
import Utils.Heap
import TI.Types


eval :: TiState -> [TiState]
eval state = state : rest_states
    where 
        rest_states | tiFinal state = []
                    | otherwise = eval next_state
        next_state = doAdmin (step state)

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], [], heap, globals, stats) = isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode (NData _ _) = True
isDataNode node = False

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

step state = dispatch (hLookup heap (head stack))
    where
        (stack, dump, heap, globals, stats) = state
        dispatch (NNum n) = numStep state n
        dispatch (NAp a1 a2) = apStep state a1 a2
        dispatch (NInd a) = indStep state a
        dispatch (NSupercomb sc args body) = scStep state sc args body
        dispatch (NPrim name prim) = primStep prim state
        dispatch (NData tag components) = dataStep state tag components

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

dataStep :: TiState -> Int -> [Addr] -> TiState
dataStep = undefined

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


applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) = (stack, dump, heap, sc_defs, stats_fun stats)
