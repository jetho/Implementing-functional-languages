module GMachine.Compiler
where

import Core.Language
import Data.List (mapAccumL)
import Utils.Assoc
import Utils.Heap
import GMachine.Types


type GmCompiledSC = (Name, Int, GmCode)
type GmCompiler = CoreExpr -> GmEnvironment -> GmCode
type GmEnvironment = ASSOC Name Int


compile :: CoreProgram -> GmState
compile program = (initialCode, [], heap, globals, statInitial)
  where (heap, globals) = buildInitialHeap program

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap program = mapAccumL allocateSc hInitial compiled
  where compiled = map compileSc program

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr)
  where (heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [PushGlobal "main", Unwind]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body) = (name, length env, compileR body (zip env [0..]))

compileR :: GmCompiler
compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]

compileC :: GmCompiler
compileC (EVar v) env
  | elem v (aDomain env)  = [Push n]
  | otherwise             = [Pushglobal v]
  where n = aLookup env v (error "Can’t happen")
compileC (ENum n) env     = [Pushint n]
compileC (EAp e1 e2) env  = compileC e2 env ++
                            compileC e1 (argOffset 1 env) ++
                            [MkAp]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env]

