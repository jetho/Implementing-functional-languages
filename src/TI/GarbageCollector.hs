module TI.GarbageCollector
where

import TI.Types
import Utils.Heap
import Data.List (foldl')


gc :: TiState -> TiState
gc (stack, dump, heap, globals, stats) = (stack, dump, heap', globals, stats)
    where
        heap' = scanHeap . foldl' markFrom heap $ roots
        roots = findStackRoots stack ++ findDumpRoots dump ++ findGlobalRoots globals

scanHeap :: TiHeap -> TiHeap
scanHeap heap = foldl' analyse heap addrs
    where
        addrs = hAddresses heap

analyse :: TiHeap -> Addr -> TiHeap
analyse heap addr =
    case node of
        NMarked n -> hUpdate heap addr n
        _ -> hFree heap addr
    where
        node = hLookup heap addr

markFrom :: TiHeap -> Addr -> TiHeap
markFrom heap addr = 
    case node of
        NMarked _ -> heap
        otherwise -> mark heap' node
    where
        node = hLookup heap addr
        heap' = hUpdate heap addr $ NMarked node

mark :: TiHeap -> Node -> TiHeap
mark heap (NInd a) = markFrom heap a
mark heap (NAp a1 a2) = markFrom (markFrom heap a1) a2
mark heap (NData _ as) = foldl' markFrom heap as 
mark heap _ = heap


findStackRoots :: TiStack -> [Addr]
findStackRoots = id

findDumpRoots :: TiDump -> [Addr]
findDumpRoots = concatMap findStackRoots

findGlobalRoots :: TiGlobals -> [Addr]
findGlobalRoots = map snd

