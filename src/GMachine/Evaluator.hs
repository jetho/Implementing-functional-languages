module GMachine.Evaluator
where

import Core.Language
import Utils.Assoc
import Utils.Heap
import GMachine.Types


eval :: GmState -> [GmState]
eval state = state : rest_states
    where 
        restStates | gmFinal state = []
                   | otherwise = eval nextState
        nextState = doAdmin (step state)

gmFinal :: GmState -> Bool
gmFinal = null . gmCode

doAdmin :: TiState -> TiState
doAdmin s = s { gmStats = statIncSteps (gmStats s) } 
