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
  where compiled = map compileSc (preludeDefs ++ program) ++ compiledPrimitives

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
  where (heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode
initialCode = [PushGlobal "main", Eval]

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

compileLetrec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetrec comp defs expr args
  = [Alloc n] ++ defs' ++ comp expr args' ++ [Slide n]
    where n     = length defs
          args' = compileArgs defs args
          defs' = concatMap (\(i, def) -> compileC def args' ++ [Update i])
                  (zip [n-1 .. 0] (map snd defs))

compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env
  = zip (map fst defs) [n-1 .. 0] ++ argOffset n env
  where n = length defs

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env]

allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes 0 heap = (heap, [])
allocNodes n heap = (heap2, a:as)
                    where (heap1, as) = allocNodes (n-1) heap
                          (heap2, a)  = hAlloc heap1 (NInd hNull)
compiledPrimitives :: [GmCompiledSC]
compiledPrimitives
  = [("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),
     ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind]),
     ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind]),
     ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind]),
     ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind]),
     ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind]),
     ("~=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind]),
     ("<",  2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind]),
     ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind]),
     (">",  2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind]),
     (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind]),
     ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
    ]

