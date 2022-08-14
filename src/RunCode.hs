
module RunCode (runCode) where

import Action (Action)
import Code (Code(..),Loc(..),CompiledRoutine(..),Chunk(..),Prog(..),Atom(..),Expression(..),Identifier(..),Binding(..),Label)
import Data.Bits (shiftL)
import Data.Dynamic (Typeable,Dynamic,toDyn,fromDynamic)
import Data.Map (Map)
import Decode (ztext)
import Dictionary (Dict)
import Eff (StatusInfo(..))
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Byte,Addr,Value)
import Story (Story(header),readStoryByte,OOB_Mode(..))
import Text.Printf (printf)
import qualified Action as A (Action(..),StatusLine(..))
import qualified Data.Map as Map
import qualified Primitive as Prim

runCode :: Byte -> Word -> Story -> Code -> Action
runCode screenWidth seed story code = do
  let q = makeState screenWidth seed story code
  let Header{initialPC} = Story.header story
  let start = LocOp initialPC
  runLoc q start

runLoc :: State -> Loc Addr -> Action
runLoc q@State{chunks} loc = do
  let chunk = maybe (error (show ("runLoc",loc))) id $ Map.lookup loc chunks
  runChunk q chunk

runChunk :: State -> Chunk -> Action
runChunk q Chunk{body} = do
  let k0 = error "runChunk didn't transfer control"
  runProg q body k0

runProg :: State -> Prog -> (State -> Action) -> Action
runProg q prog0 k = case prog0 of
  Null -> do
    k q
  Quit -> do
    let State{lastCount,count} = q
    A.Stop (count-lastCount)
  Error s -> do
    undefined s
  Labelled label p -> do
    let State{labelledPrograms=lp} = q
    runProg q { labelledPrograms = Map.insert label p lp } p k
  Goto label -> do
    let State{labelledPrograms=lp} = q
    let p = maybe (error (show ("Goto",label))) id $ Map.lookup label lp
    runProg q p k
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

runAtom :: State -> Atom -> (State -> Action) -> Action
runAtom q atom0 k = case atom0 of
  TraceOperation e op -> do
    let State{count} = q
    A.TraceInstruction "state-string" count (eval q e) op $ do
      k q { count = count + 1 }
  SetByte a b -> do
    let State{overrides} = q
    k q { overrides = Map.insert (eval q a) (eval q b) overrides }
  Note mes -> do
    A.Debug ("Note: " ++ mes) $ k q
  GamePrint mes -> do
    A.Output (eval q mes) $ k q
  MakeRoutineFrame{} -> k q --TODO
  PushFrame -> do
    let State{stack,locals,frames} = q
    k q { frames = Frame{stack,locals} : frames, stack = [], locals = Map.empty }
  PopFrame -> do
    let State{frames} = q
    case frames of
      [] -> error "PopFrame/frames=[]"
      Frame{stack,locals} : frames -> do
        k q { stack, locals, frames }
  PushReturnAddress e  -> do
    let State{callstack} = q
    k q { callstack = eval q e : callstack }
  PopReturnAddress x -> do
    let State{callstack} = q
    case callstack of
      [] -> error "PopReturnAddress/callstack=[]"
      addr:callstack -> do
        k $ (bind q x addr) { callstack }
  PushStack e -> do
    let State{stack} = q
    k q { stack = eval q e : stack }
  PopStack x -> do
    let State{stack} = q
    case stack of
      [] -> error "PopStack/stack=[]"
      v:stack -> do
        k $ (bind q x v) { stack }
  GetLocal e x -> do
    let State{locals} = q
    let n = eval q e
    let v = maybe (error (show ("GetLocal",n))) id $ Map.lookup n locals
    k $ bind q x v
  SetLocal n v -> do
    let State{locals} = q
    k q { locals = Map.insert (eval q n) (eval q v) locals }
  ReadInputFromUser statusInfo x -> do
    let State{count,lastCount} = q
    let
      statusLineM =
        case statusInfo of
          Nothing -> Nothing
          Just (StatusInfo{room,score,turns}) ->
            Just (A.StatusLine
                   { left = eval q room
                   , right = printf "score:%s--turns:%s" (show score) (show turns)
                   })
    A.Input statusLineM (count-lastCount) $ \response -> do
      k $ (bind q x response) { lastCount = count }
  StringBytes e x -> do
    let a0 = Prim.evalP1 Prim.StringBytes (eval q e)
    let a = map Const a0 -- TODO: hmm -- is this really needed?
    k $ bind q x a
  Tokenize e (x,y,z) -> do
    let State{dict} = q
    let (a,b0,c0) = Prim.evalP1 (Prim.Tokenize dict) (eval q e)
    let b = map Const b0 -- TODO: hmm
    let c = map Const c0 -- TODO: hmm
    k $ bind (bind (bind q x a) y b) z c
  LetRandom name eRange -> do
    let range = eval q eRange
    let State{seed} = q
    let x = stepRandom seed
    let result = x `mod` (fromIntegral range) + 1
    k $ (bind q name (fromIntegral result)) { seed = x }
  Let (Binding x e) -> do
    k $ bind q x (eval q e)
  Assign (Binding x e) -> do
    k $ bind q x (eval q e)
  SetNumberActuals n -> do
    k q { numActuals = eval q n }
  SetResult v ->
    k q { callResult = Just (eval q v) }


bind :: Typeable x => State ->  Identifier x -> x -> State
bind q@State{bindings} x v = do q { bindings = extendB bindings x v }


eval :: State -> Expression a -> a
eval q = \case
  Join e -> do
    eval q (eval q e) -- TODO: correctly typed, but why do I need this???
  Const x ->
    x
  NumActuals -> do
    let State{numActuals} = q
    numActuals
  CallResult -> do
    let State{callResult} = q
    maybe (error "callResult=Nothing") id callResult
  Variable x -> do
    let State{bindings} = q
    maybe (error (show ("eval/Variable",x))) id $ lookupB bindings x
  Unary p1 e1 -> do
    Prim.evalP1 p1 (eval q e1)
  Binary p2 e1 e2 -> do
    Prim.evalP2 p2 (eval q e1) (eval q e2)
  GetByteE e -> do
    let n = eval q e
    let State{story,overrides} = q
    case Map.lookup n overrides of
      Just x -> x
      Nothing -> readStoryByte (oob "eval/GetByteE") story n
  GetTextE e -> do -- TODO: can we do all these potential decodes ahead of time!
    let State{story} = q
    let (text,_) = runFetch (oob "eval/GetTextE") (eval q e) story ztext
    text
  LookupInDictE e -> do
    let State{dict} = q
    Prim.evalP1 (Prim.LookupInDict dict) (eval q e)
  Ite i t e -> do
    eval q (if (eval q i) then t else e)

  where
    oob who = OOB_Error ("RunCode.eval:"++who)

data State = State
  { chunks :: Map (Loc Addr) Chunk
  , story :: Story
  , dict :: Dict
  , lastCount :: Int
  , count :: Int
  , stack :: [Value]
  , locals :: Map Byte Value
  , frames :: [Frame]
  , callstack :: [Addr]
  , overrides :: Map Addr Byte
  , seed :: Word
  , numActuals :: Byte
  , callResult :: Maybe Value
  , labelledPrograms :: Map Label Prog
  , bindings :: Bindings
  }

data Frame = Frame
  { stack :: [Value]
  , locals :: Map Byte Value
  -- , numActuals :: Byte -- TODO: do we also need this?
  }

makeState :: Byte -> Word -> Story -> Code -> State
makeState screenWidth seed story code = do
  let Code{compiledRoutines=rs,dict} = code
  let chunks = Map.fromList
        [ (label,chunk)
        | CompiledRoutine{chunks=rChunks} <- rs
        , chunk@Chunk{label} <- rChunks
        ]
  State
    { chunks
    , story
    , dict
    , lastCount = 0
    , count = 0
    , stack = []
    , locals = Map.empty
    , frames = []
    , callstack = []
    , overrides = Map.fromList [ (0x21,screenWidth) ]
    , seed
    , numActuals = 0
    , callResult = Nothing
    , labelledPrograms = Map.empty
    , bindings = emptyB
    }

data Bindings = Bindings (Map Int Dynamic) -- Hetrogenous Map

emptyB :: Bindings
emptyB = Bindings Map.empty

lookupB :: Typeable a => Bindings -> Identifier a -> Maybe a
lookupB (Bindings m) (Identifier _ u) = Map.lookup u m >>= fromDynamic

extendB :: Typeable a => Bindings -> Identifier a -> a -> Bindings
extendB (Bindings m) (Identifier _ u) x = Bindings (Map.insert u (toDyn x) m)

-- pulled from wikipedia "Linear congruential generator"
stepRandom :: Word -> Word -- TODO: copied from Interpreter; dedup!
stepRandom x = (x * a  + c) `mod` m
  where
    a = 1103515245
    c = 12345
    m = 1 `shiftL` 31
