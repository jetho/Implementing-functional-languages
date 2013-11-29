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
dispatch (PushInt n) = pushInt n
dispatch MkAp = mkAp
dispatch (Push n) = push n
dispatch (Slide n) = slide n
dispatch Unwind = unwind

pushGlobal :: Name -> GmState -> GmState
pushGlobal f state = state { gmStack = addr: gmStack state }
    where
        addr = aLookup (gmGlobals state) f err 
        err = error $ "Undeclared global: " ++ f

pushInt :: Int -> GmState -> GmState
pushInt n state = state { gmStack = addr: gmStack state, gmHeap = heap' }
    where
        (heap', addr) = hAlloc (gmHeap state) $ NNum n

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

slide :: Int -> GmState -> GmState
slide n state = state { gmStack = a: drop n as }
    where
        a:as = gmStack state

unwind = undefined
