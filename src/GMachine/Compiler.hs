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
compile program =
    GmState { gmCode = initialCode,
              gmStack = [],
              gmDump = [],
              gmHeap = heap,
              gmGlobals = globals,
              gmStats = initialStats
            }
  where
    (heap, globals) = buildInitialHeap program

initialStats :: GmStats
initialStats = 0

buildInitialHeap :: [CoreScDefn] -> (GmHeap, GmGlobals)
buildInitialHeap program = mapAccumL allocateSc hInitial compiled
  where compiled = map compileSc program

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
  where (heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [PushGlobal "main", Unwind]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body) = (name, length env, compileR body (zip env [0..]))

compileR :: GmCompiler
compileR e env = compileC e env ++ [Update n, Pop n, Unwind]
  where n = length env

compileC :: GmCompiler
compileC (EVar v) env
  | elem v (aDomain env)  = [Push n]
  | otherwise             = [PushGlobal v]
  where n = aLookup env v (error "Can’t happen")
compileC (ENum n) env     = [PushInt n]
compileC (EAp e1 e2) env  = compileC e2 env ++
                            compileC e1 (argOffset 1 env) ++
                            [MkAp]
compileC (ELet recursive defs e) args
  | recursive             = compileLetrec compileC defs e args
  | otherwise             = compileLet    compileC defs e args


compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLet comp defs expr env
  = compileLet' defs env ++ comp expr env' ++ [Slide (length defs)]
  where env' = compileArgs defs env

compileLet' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLet' []                  env = []
compileLet' ((name, expr):defs) env
  = compileC expr env ++ compileLet' defs (argOffset 1 env)

compileLetrec = undefined

compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env
  = zip (map fst defs) [n-1, n-2 .. 0] ++ argOffset n env
  where n = length defs

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env]

