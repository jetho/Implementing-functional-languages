module TI.Main
where

import Core.Parser
import Core.PrettyPrinter
import Utils.Heap
import Data.List (sort)
import TI.Types
import TI.Compiler
import TI.Evaluator


runProg :: String -> String
runProg = showResults . eval . compile . parse

showResults :: [TiState] -> String
showResults states = iDisplay $ iConcat [ iLayn (map showState states), showStats (last states) ]

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
showNode (NData tag addrs) = 
    iConcat [ iStr ("NData: " ++ show tag ++ " "),
              iInterleave (iStr " ") $ map (iStr . showAddr) addrs ] 

showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr $ space (4 - length str) ++ str
    where 
        str = show addr

showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats) = 
    iConcat [ iNewline, iNewline, iStr "Total number of steps = ", iNum (tiStatGetSteps stats) ]

