
module RunCode (runCode) where

import Action (Action)
import Code (Code(..),Loc(..),CompiledRoutine(..),Chunk(..),Prog(..),Atom(..),Expression(..),Identifier(..),Binding(..))
import Data.Dynamic (Typeable,Dynamic,toDyn,fromDynamic)
import Data.Map (Map)
import Numbers (Byte,Addr,Value)
import Story (Story,readStoryByte,OOB_Mode(..))
import qualified Action as A (Action(..))
import qualified Data.Map as Map
import qualified Primitive as Prim

runCode :: Story -> Word -> Code -> Action
runCode story _seed code = do
  let start = getStart code
  let static = makeStaticEnv story code
  let q = makeEnv static
  runLoc q start

getStart :: Code -> Loc Addr
getStart _ = LocOp 20229 -- TODO: hack zork start address

runLoc :: Env -> Loc Addr -> Action
runLoc q@Env{static} loc = do
  runChunk q (findChunk static loc)

runChunk :: Env -> Chunk -> Action
runChunk q Chunk{body} = do
  let k0 = error "runChunk didn't transfer control"
  runProg q body k0

runProg :: Env -> Prog -> (Env -> Action) -> Action
runProg q prog0 k = case prog0 of
  Null -> do
    k q
  Quit -> do
    undefined
  Error s -> do
    undefined s
  Labelled label p -> do
    undefined label p
  Goto label -> do
    undefined label
  JumpIndirect loc -> do
    undefined loc -- TODO: here
  Jump loc -> do
    runLoc q loc
  Seq atom prog -> do
    runAtom q atom $ \q -> runProg q prog k
  FullSeq p1 p2 -> do
    runProg q p1 $ \q -> runProg q p2 k
  If i t e -> do
    runProg q (if (eval q i) then t else e) k

runAtom :: Env -> Atom -> (Env -> Action) -> Action
runAtom q atom0 k = case atom0 of
  TraceOperation e op -> do
    let Env{count} = q
    A.TraceInstruction "xx" count (eval q e) op $ do
      k q { count = count + 1 }
  SetByte a b -> do
    let Env{overrides} = q
    k q { overrides = Map.insert (eval q a) (eval q b) overrides }
  Note{} -> undefined
  GamePrint{} -> undefined
  MakeRoutineFrame{} -> k q --TODO
  PushFrame -> k q-- TODO
  PopFrame -> k q -- TODO
  PushReturnAddress{} -> k q -- TODO
  PopReturnAddress{} -> k q -- TODO
  PushStack{} -> undefined
  PopStack{} -> undefined
  SetLocal n v -> do
    let Env{locals} = q
    k q { locals = Map.insert (eval q n) (eval q v) locals }
  ReadInputFromUser{} -> undefined
  StringBytes{} -> undefined
  Tokenize{} -> undefined
  LetRandom{} -> undefined
  Let (Binding x e) -> do
    let Env{bindings} = q
    k q { bindings = extendB bindings x (eval q e) }
  Assign{} -> undefined
  SetNumberActuals{} -> undefined
  SetResult{} -> k q -- TODO

eval :: Env -> Expression a -> a
eval q = \case
  Join{} -> do
    undefined
  Const x ->
    x
  NumActuals -> do
    undefined q
  CallResult -> do
    undefined
  Variable x -> do
    let Env{bindings} = q
    maybe (error (show ("eval/Variable",x))) id $ lookupB bindings x
  Unary p1 e1 -> do
    Prim.evalP1 p1 (eval q e1)
  Binary p2 e1 e2 -> do
    Prim.evalP2 p2 (eval q e1) (eval q e2)
  GetByteE e -> do
    let n = eval q e
    let Env { static = StaticEnv{story}, overrides } = q
    case Map.lookup n overrides of
      Just{} -> undefined -- TODO
      Nothing -> readStoryByte (oob "eval/GetByteE") story n
  GetLocalE e -> do
    let Env{locals} = q
    let n = eval q e
    maybe (error (show ("eval/GetLocalE",n))) id $ Map.lookup n locals
  GetTextE e -> do
    undefined e
  LookupInDictE e -> do
    undefined e
  Ite i t e -> do
    undefined i t e

  where
    oob who = OOB_Error ("RunCode.eval:"++who)

data Env = Env
  { static :: StaticEnv
  , locals :: Map Byte Value
  , bindings :: Bindings
  , overrides :: Map Addr Byte
  , count :: Int
  }

makeEnv :: StaticEnv -> Env
makeEnv static = Env
  { static
  , locals = Map.empty
  , bindings = emptyB
  , overrides = Map.empty
  , count = 0
  }

data Bindings = Bindings (Map Int Dynamic) -- Hetrogenous Map

emptyB :: Bindings
emptyB = Bindings Map.empty

lookupB :: Typeable a => Bindings -> Identifier a -> Maybe a
lookupB (Bindings m) (Identifier _ u) = Map.lookup u m >>= fromDynamic

extendB :: Typeable a => Bindings -> Identifier a -> a -> Bindings
extendB (Bindings m) (Identifier _ u) x = Bindings (Map.insert u (toDyn x) m)

data StaticEnv = StaticEnv
  { m :: Map (Loc Addr) Chunk
  , story :: Story
  }

makeStaticEnv :: Story -> Code -> StaticEnv
makeStaticEnv story Code{routines} = do
  let m = Map.fromList
        [ (label,chunk)
        | CompiledRoutine{chunks} <- routines
        , chunk@Chunk{label} <- chunks
        ]
  StaticEnv { m, story }

findChunk :: StaticEnv -> Loc Addr -> Chunk
findChunk StaticEnv{m} k = do
  maybe (error (show ("findChunk",k))) id $ Map.lookup k m
