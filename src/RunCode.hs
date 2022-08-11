
module RunCode (runCode) where

import Action (Action)
import Code (Code(..),Loc(..),CompiledRoutine(..),Chunk(..),Prog(..),Atom(..),Expression(..))
import Data.Map (Map)
import Numbers (Addr)
import qualified Action as A (Action(..))
import qualified Data.Map as Map

runCode :: Word -> Code -> Action
runCode _seed code = do
  let start = getStart code
  let static = makeStaticEnv code
  let q = makeEnv static
  runLoc q start

getStart :: Code -> Loc Addr
getStart _ = LocOp 20229 -- TODO: hack zork start address; get real start from Code

runLoc :: Env -> Loc Addr -> Action
runLoc q@Env{static} loc = do
  runChunk q (findChunk static loc)

runChunk :: Env -> Chunk -> Action
runChunk q Chunk{body} = do
  let k0 = error "runChunk didn't transfer control"
  runProg q body k0

runProg :: Env -> Prog -> Action -> Action
runProg q prog0 k = case prog0 of
  Null -> do
    k
  Quit -> do
    undefined
  Error s -> do
    undefined s
  Labelled label p -> do
    undefined label p
  Goto label -> do
    undefined label
  JumpIndirect loc -> do
    undefined loc
  Jump loc -> do
    runLoc q loc
  Seq atom prog -> do
    runAtom q atom (runProg q prog k)
  FullSeq p1 p2 -> do
    runProg q p1 (runProg q p2 k)
  If{} -> do
    undefined

runAtom :: Env -> Atom -> Action -> Action
runAtom q atom0 k = case atom0 of
  TraceOperation e op -> do
    A.TraceInstruction "xx" 0 (eval q e) op $ do --TODO: #instr
      k
  SetByte{} -> undefined
  Note{} -> undefined
  GamePrint{} -> undefined
  MakeRoutineFrame{} -> k --TODO
  PushFrame -> k -- TODO
  PopFrame -> undefined
  PushReturnAddress{} -> k -- TODO
  PopReturnAddress{} -> undefined
  PushStack{} -> undefined
  PopStack{} -> undefined
  SetLocal{} -> k -- TODO
  ReadInputFromUser{} -> undefined
  StringBytes{} -> undefined
  Tokenize{} -> undefined
  LetRandom{} -> undefined
  Let{} -> undefined -- TODO, Here
  Assign{} -> undefined
  SetNumberActuals{} -> undefined
  SetResult{} -> undefined

eval :: Env -> Expression a -> a
eval q = \case
  Const x -> x
  _ -> undefined q

data Env = Env
  { static :: StaticEnv
  }

makeEnv :: StaticEnv -> Env
makeEnv static = Env { static }

data StaticEnv = StaticEnv
  { m :: Map (Loc Addr) Chunk
  }

makeStaticEnv :: Code -> StaticEnv
makeStaticEnv Code{routines} = do
  StaticEnv $ Map.fromList
    [ (label,chunk)
    | CompiledRoutine{chunks} <- routines
    , chunk@Chunk{label} <- chunks
    ]

findChunk :: StaticEnv -> Loc Addr -> Chunk
findChunk StaticEnv{m} k = do
  maybe (error (show ("findChunk",k))) id $ Map.lookup k m
