
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

runCode :: Byte -> Word -> Story -> Code -> Action
runCode screenWidth _seed story code = do
  let start = getStart code
  let static = makeStaticEnv story code
  let q = makeEnv screenWidth static
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
    let _ = undefined label -- TODO
    runProg q p k
  Goto label -> do
    undefined label
  JumpIndirect loc -> do
    runLoc q (eval q <$> loc)
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
    A.TraceInstruction "xx" count (eval q e) op $ do -- TODO: kill state-string
      k q { count = count + 1 }
  SetByte a b -> do
    let Env{overrides} = q
    k q { overrides = Map.insert (eval q a) (eval q b) overrides }
  Note{} -> do
    undefined
  GamePrint mes -> do
    A.Output (eval q mes) $ k q
  MakeRoutineFrame{} -> k q --TODO
  PushFrame -> k q-- TODO
  PopFrame -> k q -- TODO
  PushReturnAddress e  -> do
    let Env{callstack} = q
    k q { callstack = eval q e : callstack }
  PopReturnAddress x -> do
    let Env{callstack} = q
    case callstack of
      [] -> error "callstack[]"
      addr:callstack -> do
        k $ (bind q x addr) { callstack }
  PushStack e -> do
    let Env{stack} = q
    k q { stack = eval q e : stack }
  PopStack x -> do
    let Env{stack} = q
    case stack of
      [] -> error "stack[]"
      v:stack -> do
        k $ (bind q x v) { stack }
  GetLocal e x -> do
    let Env{locals} = q
    let n = eval q e
    let v = maybe (error (show ("eval/GetLocalE",n))) id $ Map.lookup n locals
    k $ bind q x v
  SetLocal n v -> do
    let Env{locals} = q
    k q { locals = Map.insert (eval q n) (eval q v) locals }
  ReadInputFromUser{} -> do
    undefined
  StringBytes{} -> do
    undefined
  Tokenize{} -> do
    undefined
  LetRandom{} -> do
    undefined
  Let (Binding x e) -> do
    k $ bind q x (eval q e)
  Assign{} -> do
    undefined -- TODO, here
  SetNumberActuals n -> do
    let _ = undefined n -- TODO
    k q
  SetResult v ->
    k q { callResult = Just (eval q v) }


bind :: Typeable x => Env ->  Identifier x -> x -> Env
bind q@Env{bindings} x v = do q { bindings = extendB bindings x v }


eval :: Env -> Expression a -> a
eval q = \case
  Join{} -> do
    undefined
  Const x ->
    x
  NumActuals -> do
    undefined q
  CallResult -> do
    let Env{callResult} = q
    maybe (error "callResult=Nothing") id callResult
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
      Just x -> x
      Nothing -> readStoryByte (oob "eval/GetByteE") story n
  GetTextE e -> do
    undefined e
  LookupInDictE e -> do
    undefined e
  Ite i t e -> do
    undefined i t e

  where
    oob who = OOB_Error ("RunCode.eval:"++who)

data Env = Env -- TODO: rename State?
  { static :: StaticEnv
  , locals :: Map Byte Value
  , bindings :: Bindings
  , overrides :: Map Addr Byte
  , count :: Int
  , callstack :: [Addr]
  , stack :: [Value]
  , callResult :: Maybe Value
  }

makeEnv :: Byte -> StaticEnv -> Env
makeEnv screenWidth static = Env
  { static
  , locals = Map.empty
  , bindings = emptyB
  , overrides = Map.fromList [ (0x21,screenWidth) ]
  , count = 0
  , callstack = []
  , stack = []
  , callResult = Nothing
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
