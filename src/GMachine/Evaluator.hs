module GMachine.Evaluator
where

import Core.Language
import Utils.Assoc
import Utils.Heap
import GMachine.Types


eval :: GmState -> [GmState]
eval state = state : restStates
    where
        restStates | gmFinal state = []
                   | otherwise = eval nextState
        nextState = doAdmin (step state)

gmFinal :: GmState -> Bool
gmFinal = null . gmCode

doAdmin :: GmState -> GmState
doAdmin state = state { gmStats = statIncSteps $ gmStats state }

step :: GmState -> GmState
step state = dispatch i $ state { gmCode = is}
    where
        (i:is) = gmCode state

dispatch :: Instruction -> GmState -> GmState
dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n)    = pushInt n
dispatch MkAp           = mkAp
dispatch (Push n)       = push n
dispatch (Update n)     = update n
dispatch (Pop n)        = pop n
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind
dispatch Eval           = evalOp
dispatch Add            = arithmetic2 (+)
dispatch Sub            = arithmetic2 (-)
dispatch Mul            = arithmetic2 (*)
dispatch Div            = arithmetic2 (div)
dispatch Neg            = arithmetic1 negate

pushGlobal :: Name -> GmState -> GmState
pushGlobal f state = state { gmStack = addr : gmStack state }
    where
        addr = aLookup (gmGlobals state) f err
        err  = error $ "Undeclared global: " ++ f

pushInt :: Int -> GmState -> GmState
pushInt n state =
    case aLookup globals ident (-1) of
        -1 ->
            state { gmStack = stack', gmHeap = heap', gmGlobals = globals' }
            where
                (heap', addr) = hAlloc heap $ NNum n
                stack'        = addr : stack
                globals'      = (ident, addr) : globals
        addr ->
            state { gmStack = stack' }
            where
                stack' = addr : stack
    where
        heap    = gmHeap state
        stack   = gmStack state
        globals = gmGlobals state
        ident   = show n

mkAp :: GmState -> GmState
mkAp state = state { gmStack = addr:as, gmHeap = heap' }
    where
        a1:a2:as = gmStack state
        (heap', addr) = hAlloc (gmHeap state) $ NAp a1 a2

push :: Int -> GmState -> GmState
push n state = state { gmStack = stack' }
    where
        stack' = addr : stack
        addr   = stack !! n
        stack  = gmStack state

update :: Int -> GmState -> GmState
update n state = state { gmStack = as, gmHeap = heap' }
    where
        a:as  = gmStack state
        heap' = hUpdate (gmHeap state) root $ NInd a
        root  = as !! n

pop :: Int -> GmState -> GmState
pop n state = state { gmStack = drop n $ gmStack state }

slide :: Int -> GmState -> GmState
slide n state = state { gmStack = a : drop n as }
    where a:as = gmStack state

evalOp :: GmState -> GmState
evalOp state = state { gmCode = [Unwind], gmStack = [a], gmDump = d' }
   where a:as  = gmStack state
         d'    = (gmCode state, as) : gmDump state

unwind :: GmState -> GmState
unwind state = newState $ hLookup heap a
    where
        a:as                 = gmStack state
        (i',s'):d            = gmDump state
        newState (NInd addr) = state { gmCode = [Unwind], gmStack = addr:as }
        newState (NNum _)    = state { gmCode = i', gmStack = a:s', gmDump = d }
        newState (NAp a1 _)  = state { gmCode = [Unwind], gmStack = a1:a:as }
        newState (NGlobal n c)
            | length as < n  = error "Unwinding with too few arguments"
            | otherwise      = state { gmCode = c
                                     , gmStack = (rearrange n heap stack)
                                     }
        stack = gmStack state
        heap  = gmHeap state

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap stack = addrs ++ drop n stack
    where
        addrs = map (getArg . hLookup heap) (take n $ tail stack)

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2

boxInteger :: Int -> GmState -> GmState
boxInteger n state = state { gmStack = a: gmStack state, gmHeap = h' }
    where (h', a) = hAlloc (gmHeap state) (NNum n)

unboxInteger :: Addr -> GmState -> Int
unboxInteger a state = ub (hLookup (gmHeap state) a)
    where   ub (NNum i) = i
            ub n        = error "Unboxing a non-integer"

primitive1 :: (b -> GmState -> GmState)
         -> (Addr -> GmState -> a)
         -> (a -> b)
         -> (GmState -> GmState)
primitive1 box unbox op state = box (op (unbox a state)) state { gmStack = as }
    where a:as = gmStack state

primitive2 :: (b -> GmState -> GmState)
         -> (Addr -> GmState -> a)
         -> (a -> a -> b)
         -> (GmState -> GmState)
primitive2 box unbox op state
    = box (op (unbox a0 state) (unbox a1 state)) state { gmStack = as }
    where (a0:a1:as) = gmStack state

arithmetic1 ::   (Int -> Int) -> (GmState -> GmState)
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 ::   (Int -> Int -> Int) -> (GmState -> GmState)
arithmetic2 = primitive2 boxInteger unboxInteger

