
module Walk (traceExecution,dumpObjects) where

import Data.Bits ((.&.))
import Data.Map (Map)
import Decode (fetchInstruction,fetchRoutineHeader,ztext)
import Eff (Eff(..),Bin(..))
import Evaluation (theEffect)
import Fetch (runFetch)
import Instruction (Target)
import Interaction (Inter(..),Stats(..),runInter)
import Numbers (Byte,Addr,Value)
import Story (Story,readStoryByte)
import qualified Data.Map as Map
import qualified Interaction (Conf(..))
import qualified Objects (dump)

traceExecution :: Interaction.Conf -> Story -> [String] -> IO ()
traceExecution conf story inputs = do
  let maxSteps = 395
  let e = theEffect
  let s :: State = initState story
  let i :: Inter = runEff maxSteps s e
  runInter conf inputs i

dumpObjects :: Story -> IO ()
dumpObjects story = do
  let maxSteps = 1000
  let e = Objects.dump
  let s :: State = initState story
  let i :: Inter = runEff maxSteps s e
  let conf = Interaction.Conf { debug = True, seeStats = False }
  runInter conf [] i

--[interpreter for execution effect]----------------------------------

runEff :: Int -> State -> Eff () -> Inter
runEff maxSteps s0 e0 = loop s0 e0 $ \_ () -> I_Stop
  where
    loop :: State -> Eff a -> (State -> a -> Inter) -> Inter
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      GamePrint mes -> I_Output mes (k s ())
      Debug a -> I_Debug (show a) (k s ())

      ReadInputFromUser -> do
        let State{count} = s
        I_Input count $ \response -> k s response

      GetText a -> do
        let State{story,stats} = s
        let (text,_,readCount) = runFetch a story ztext
        let Stats{ct} = stats
        let s' = s { stats = stats { ct = ct + readCount }}
        k s' text

      FetchI -> do
       --I_Output (show s) $ do
        let State{story,pc,count,stats} = s
        let (ins,pc',readCount) = runFetch pc story fetchInstruction
        if count >= maxSteps then I_Stop else do
          let Stats{ct} = stats
          let s' = s { pc = pc'
                     , count = count + 1
                     , stats = stats { ct = ct + readCount }
                     }
          I_Trace stats count pc ins (k s' ins)

      FetchHeader{} -> do
        let State{story,pc,stats} = s
        let (rh,pc',readCount) = runFetch pc story fetchRoutineHeader
        let Stats{ct} = stats
        let s' = s { pc = pc', stats = stats { ct = ct + readCount} }
        k s' rh

      PushFrame addr target -> do
        let State{pc,stack,locals,frames} = s
        k s { pc = addr, frames = Frame { pc, target, stack, locals } : frames } ()

      PopFrame -> do
        let State{frames} = s
        case frames of
          [] -> error "PopFrame, frames=[]"
          Frame{pc,target,stack,locals}:frames -> do
            k s { pc, stack, locals, frames } target

      SetPC pc -> k s { pc } ()

      GetLocal n -> do
        let State{locals} = s
        let v = maybe (error (show ("GetLocal",n))) id $ Map.lookup n locals
        k s v

      SetLocal n v -> do
        let State{locals} = s
        k s { locals = Map.insert n v locals } ()

      EqualAny vs -> do
        let
          res =
            case vs of
              [v1,v2] -> v1==v2
              [v1,v2,v3] -> v1==v2 || v1==v3
              vs -> error (show ("EqualAny",vs))
        k s res

      IsZero value -> do
        let res = (value == 0)
        k s res

      BinOp bin v1 v2 -> do
        let res = case bin of
              BAdd -> v1 + v2
              BSub -> v1 - v2
              BAnd -> v1 .&. v2
        k s res

      GetByte a -> do
        let State{story,overrides,stats} = s
        let (_over,b) =
              case Map.lookup a overrides of
                Just b -> (True,b)
                Nothing -> (False,readStoryByte story a)
        let Stats{rt} = stats
        k s { stats = stats { rt = rt + 1}} b

      SetByte a b -> do
        let State{overrides} = s
        k s { overrides = Map.insert a b overrides } ()

      PushStack v -> do
        let State{stack} = s
        k s { stack = v : stack } ()

      PopStack -> do
        let State{stack} = s
        case stack of
          [] -> error "PopStack: []"
          v:stack -> do
            k s { stack } v

--[interpreter state]-------------------------------------------------

data State = State
  { story :: Story
  , pc :: Addr
  , count :: Int
  , stack :: [Value]
  , locals :: Map Byte Value
  , frames :: [Frame]
  , overrides :: Map Addr Byte
  , stats :: Stats
  }

instance Show State where
  show State{pc,stack,locals,frames} = show (pc,stack,locals,frames)

initState :: Story -> State
initState story = do
  let pc :: Addr = fromIntegral (readStoryWord story 0x6)
  State { story
        , pc
        , count = 0
        , stack = []
        , locals = Map.empty
        , frames = []
        , overrides = Map.empty
        , stats = Stats { ct = 0, rt = 0 }
        }

readStoryWord :: Story -> Addr -> Word
readStoryWord story a = do
  let hi = readStoryByte story a
  let lo = readStoryByte story (a+1)
  256 * fromIntegral hi + fromIntegral lo

data Frame = Frame
  { pc :: Addr
  , target :: Target
  , stack :: [Value]
  , locals :: Map Byte Value
  }
  deriving Show
