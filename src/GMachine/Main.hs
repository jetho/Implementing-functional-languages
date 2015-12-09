module GMachine.Main
where


import Core.Language
import Core.Parser
import Core.PrettyPrinter
import GMachine.Types
import GMachine.Compiler
import GMachine.Evaluator
import Utils.Heap


runProg :: String -> String
runProg = showResults . eval . compile . parse


showResults :: [GmState] -> String
showResults states
      = iDisplay (iConcat [
      iStr "Supercombinator definitions", iNewline,
      iInterleave iNewline (map (showSC s) (gmGlobals s)),
      iNewline, iNewline, iStr "State transitions", iNewline, iNewline,
      iLayn (map showState states),
      iNewline, iNewline,
      showStats (last states)])
      where (s:ss) = states

showSC :: GmState -> (Name, Addr) -> Iseq
showSC s (name, addr)
      = iConcat [ iStr "Code for ", iStr name, iNewline,
            showInstructions code, iNewline, iNewline]
      where (NGlobal arity code) = (hLookup (gmHeap s) addr)

showInstructions :: GmCode -> Iseq
showInstructions is
      = iConcat [iStr "  Code:{",
           iIndent (iInterleave iNewline (map showInstruction is)),
           iStr "}", iNewline]

showInstruction Unwind         = iStr  "Unwind"
showInstruction (PushGlobal f) = (iStr "PushGlobal ") `iAppend` (iStr f)
showInstruction (Push n)       = (iStr "Push ")       `iAppend` (iNum n)
showInstruction (PushInt n)    = (iStr "PushInt ")    `iAppend` (iNum n)
showInstruction MkAp           = iStr  "MkAp"
showInstruction (Update n)     = (iStr "Update ")     `iAppend` (iNum n)
showInstruction (Pop n)        = (iStr "Pop ")        `iAppend` (iNum n)
showInstruction (Slide n)      = (iStr  "Slide ")     `iAppend` (iNum n)

showState :: GmState -> Iseq
showState s
   = iConcat [showStack s,                   iNewline,
              showDump s,                    iNewline,
              showInstructions (gmCode s),   iNewline]

showStack :: GmState -> Iseq
showStack s
      = iConcat [iStr " Stack:[",
           iIndent (iInterleave iNewline
                       (map (showStackItem s) (reverse (gmStack s)))),
           iStr "]"]

showStackItem :: GmState -> Addr -> Iseq
showStackItem s a
      = iConcat [iStr (showAddr a), iStr ": ",
           showNode s a (hLookup (gmHeap s) a)]

showNode s a (NNum n)      = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
                             where v = head [n | (n,b) <- globals, a==b]
                                   globals = gmGlobals s
showNode s a (NAp a1 a2)   = iConcat [iStr "Ap ",  iStr (showAddr a1),
                                      iStr " ",    iStr (showAddr a2)]
showNode s a (NInd a1)     = iConcat [iStr "Ind ", iStr (showAddr a1)]

showStats :: GmState -> Iseq
showStats s
      = iConcat [ iStr "Steps taken = ", iNum (statGetSteps (gmStats s))]

showDump :: GmState -> Iseq
showDump s
  = iConcat [iStr " Dump:[",
             iIndent (iInterleave iNewline
                     (map showDumpItem (reverse (gmDump s)))),
             iStr "]"]

showDumpItem :: GmDumpItem -> Iseq
showDumpItem (code, stack)
  = iConcat [iStr "<",
             shortShowInstructions 3 code, iStr ", ",
             shortShowStack stack,
             iStr ">"]

shortShowInstructions :: Int -> GmCode -> Iseq
shortShowInstructions number code
  = iConcat [iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}"]
  where   codes   = map showInstruction (take number code)
          dotcodes      | length code > number  = codes ++ [iStr "..."]
                        | otherwise             = codes

shortShowStack :: GmStack -> Iseq
shortShowStack stack
  = iConcat [iStr "[",
             iInterleave (iStr ", ") (map (iStr . showAddr) stack),
             iStr "]"]

