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
dispatch Unwind         = unwind

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
push n state = state { gmStack = addr:stack }
    where
        stack = gmStack state
        addr = getArg $ hLookup (gmHeap state) (stack !! (n+1))
        getArg (NAp _ a2) = a2

update :: Int -> GmState -> GmState
update n state = state { gmStack = as, gmHeap = heap' }
    where
        a:as  = gmStack state
        heap' = hUpdate (gmHeap state) root $ NInd a
        root  = as !! n

pop :: Int -> GmState -> GmState
pop n state = state { gmStack = drop n $ gmStack state }

unwind :: GmState -> GmState
unwind state = newState $ hLookup (gmHeap state) a
    where
        a:as                 = gmStack state
        newState (NInd addr) = state { gmCode = [Unwind], gmStack = addr:as }
        newState (NNum _)    = state
        newState (NAp a1 _)  = state { gmCode = [Unwind], gmStack = a1:a:as }
        newState (NGlobal n c)
            | length as < n  = error "Unwinding with too few arguments"
            | otherwise      = state { gmCode = c }

