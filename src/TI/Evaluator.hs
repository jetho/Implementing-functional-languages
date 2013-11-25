module TI.Evaluator
where

import Core.Language
import Utils.Assoc
import Utils.Heap
import TI.Types
import TI.GarbageCollector


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
doAdmin = applyToStats tiStatIncSteps . gc

step :: TiState -> TiState
step state = dispatch (hLookup heap (head stack))
    where
        (stack, dump, heap, globals, stats) = state
        dispatch (NNum n) = numStep state
        dispatch (NAp a1 a2) = apStep state a1 a2
        dispatch (NInd a) = indStep state a
        dispatch (NSupercomb sc args body) = scStep state sc args body
        dispatch (NPrim name prim) = primStep prim state
        dispatch (NData tag components) = dataStep state 

numStep :: TiState -> TiState
numStep ([_], stack:dump, heap, globals, stats) = (stack, dump, heap, globals, stats)
numStep _ = error "Number applied as a function!"

dataStep :: TiState -> TiState
dataStep ([_], (stack:dump), heap, globals, stats) = (stack, dump, heap, globals, stats)
dataStep _ = error "Data applied as a function!"

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
primStep Greater = primCompare (>)
primStep GreaterEq = primCompare (>=)
primStep Less = primCompare (<)
primStep LessEq = primCompare (<=)
primStep Eq = primCompare (==)
primStep NotEq = primCompare (/=)
primStep (PrimConstr t a) = primConstr t a
primStep If = primIf 
primStep CasePair = primCasePair
primStep CaseList = primCaseList
primStep Abort = primAbort
primStep unknown = error $ "Unknown Primitive: " ++ show unknown

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

primCompare :: (Int -> Int -> Bool) -> TiState -> TiState
primCompare op state@(_, _, heap, env, _) = primDyadic state cmpFun
    where 
        cmpFun (NNum n1) (NNum n2) = hLookup heap addr
            where 
                addr = aLookup env boolRes $ error $ boolRes ++ " is not defined"
                boolRes = if op n1 n2 then "True" else "False"

primConstr :: Int -> Int -> TiState -> TiState
primConstr t a (stack, dump, heap, globals, stats)
    | length stack <= a = error $ "Type error: Pack{" ++ show t ++ "," ++ show a ++ "} is applied to too few arguments."
    | otherwise = (stack', dump, heap', globals, stats)
    where 
        components = take a $ getArgs heap stack
        stack' = drop a stack
        heap' = hUpdate heap result_addr $ NData t components
        result_addr = head stack'

primIf :: TiState -> TiState
primIf (stack, dump, heap, globals, stats) 
    | length stack <= 3 = error "Incomplete If Expression!"
    | isDataNode condition = (stack', dump, heap', globals, stats)
    | otherwise = ([b], (tail stack):dump, heap, globals, stats) 
    where
        [b, t, f] = take 3 $ getArgs heap stack
        condition = hLookup heap b
        stack' = drop 3 stack
        result_addr = head stack'
        heap' = hUpdate heap result_addr branch
        branch = case condition of 
            NData 1 _ -> hLookup heap f
            NData 2 _ -> hLookup heap t
            _ -> error "invalid boolean expression"

primCasePair :: TiState -> TiState
primCasePair (stack, dump, heap, globals, stats) 
    | length stack <= 2 = error $ "Malformed casePair Expression!"
    | isPair pair = (stack', dump, heap'', globals, stats)
    | isDataNode pair = error $ "Invalid argument to casePair"
    | otherwise = ([p], (tail stack):dump, heap, globals, stats) 
    where
        [p, f] = take 2 $ getArgs heap stack
        pair = hLookup heap p
        stack' = drop 2 stack
        result_addr = head stack'
        (heap', fNode) = applyPair heap pair f
        heap'' = hUpdate heap' result_addr fNode

isPair :: Node -> Bool
isPair (NData 1 [_, _]) = True
isPair _ = False

applyPair :: TiHeap -> Node -> Addr -> (TiHeap, Node)
applyPair heap (NData 1 [fst, snd]) f = (heap'', apNode)
    where
        (heap', addr) = hAlloc heap (NAp f fst)
        (heap'', addr') = hAlloc heap' (NAp addr snd)
        apNode = hLookup heap'' addr'        

primCaseList :: TiState -> TiState
primCaseList (stack, dump, heap, globals, stats) 
    | length stack <= 3 = error $ "Malformed caseList Expression!"
    | isList list = (stack', dump, heap'', globals, stats)
    | isDataNode list = error $ "Invalid argument to caseList"
    | otherwise = ([l], (tail stack):dump, heap, globals, stats) 
    where
        [l, nilFunc, consFunc] = take 3 $ getArgs heap stack
        list = hLookup heap l
        stack' = drop 3 stack
        result_addr = head stack'
        (heap', fNode) = applyList heap list nilFunc consFunc
        heap'' = hUpdate heap' result_addr fNode

isList :: Node -> Bool
isList (NData 1 []) = True
isList (NData 2 [_, _]) = True
isList _ = False

applyList :: TiHeap -> Node -> Addr -> Addr -> (TiHeap, Node)
applyList heap (NData 1 []) f _ = (heap, hLookup heap f)
applyList heap (NData 2 [x, xs]) _ f = (heap'', apNode) 
    where
        (heap', addr) = hAlloc heap (NAp f x)
        (heap'', addr') = hAlloc heap' (NAp addr xs)
        apNode = hLookup heap'' addr'
applyList _ _ _ _ = error "List expected"

primAbort :: TiState -> TiState
primAbort = error "Computation aborted."

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
    instantiateConstr tag arity heap 
instantiate (ELet isRec defs body) heap env =
    instantiateLet isRec defs body heap env
instantiate (ECase e alts) heap env = error "Can’t instantiate case exprs"

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> ASSOC Name Addr -> TiHeap
instantiateAndUpdate (EVar v) upd_addr heap env = hUpdate heap upd_addr $ NInd $ aLookup env v $ error msg
    where 
        msg = "Undefined name " ++ show v
instantiateAndUpdate (ENum n) upd_addr heap env = hUpdate heap upd_addr (NNum n)
instantiateAndUpdate (EConstr tag arity) upd_addr heap env =
    hUpdate heap upd_addr $ NPrim "Pack" $ PrimConstr tag arity
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

instantiateConstr :: Int -> Int -> TiHeap -> (TiHeap, Addr)
instantiateConstr tag arity heap = hAlloc heap $ NPrim "Pack" $ PrimConstr tag arity


applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) = (stack, dump, heap, sc_defs, stats_fun stats)
