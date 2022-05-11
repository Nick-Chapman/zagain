
module Walk (walkZork,dumpZorkObjects) where

import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Map (Map)
import Decode (fetchInstruction,fetchRoutineHeader,ztext)
import Dis (runFetch)
import Eff (Eff(..),Bin(..))
import Evaluation (theEffect)
import Instruction (Instruction,RoutineHeader,Target)
import Numbers (Byte,Addr,Value)
import Story (Story,loadStory,readStoryByte)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Instruction as I
import qualified Objects (dump)

walkZork :: IO ()
walkZork = do
  let debug = True
  let maxSteps = 1000
  let filename = "story/zork1.88-840726.z3"
  story <- loadStory filename
  let e = theEffect
  let s :: State = initState story
  let i :: Inter = runEff maxSteps s e
  runInter debug i

dumpZorkObjects :: IO ()
dumpZorkObjects = do
  let debug = True
  let maxSteps = 1000
  putStrLn "*dumpZorkObjects*"
  let filename = "story/zork1.88-840726.z3"
  story <- loadStory filename
  let e = Objects.dump
  let s :: State = initState story
  let i :: Inter = runEff maxSteps s e
  runInter debug i

--[run interaction as IO]---------------------------------------------

runInter :: Bool -> Inter -> IO ()
runInter debug = loop []
  where
    loop :: [String] -> Inter -> IO ()
    loop buf = \case
      I_Trace _n a instruction next -> do
        printf "(Decode %d %s %s)\n" _n (show a) (I.pretty instruction)
        --printf "(Decode XXX %s %s)\n" (show a) (I.pretty instruction)
        loop buf next
      I_Output text next -> do
        loop (text:buf) next
      I_Debug s next -> do
        when (debug) $ putStrLn ("Debug: " ++ s)
        loop buf next
      I_Input _f -> do
        mapM_ putStr (reverse buf)
        putStrLn ""
        --loop buf (_f "text from user") -- TODO: get text from user
        pure ()
      I_Stop -> do
        pure ()

--[interaction type]--------------------------------------------------

data Inter
  = I_Trace Int Addr Instruction Inter
  | I_Output String Inter
  | I_Debug String Inter
  | I_Input (String -> Inter) --TODO: make use of this!
  | I_Stop

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

      ReadInputFromUser -> do I_Input $ \response -> k s response

      GetText a -> do
        let State{story} = s
        let (text,_) =
              case (runFetch a story ztext) of
                Left e -> error (show ("GetText",e))
                Right x -> x
        k s text

      FetchI -> do
       --I_Output (show s) $ do
        let State{story,pc,count} = s
        let (ins,pc') = fetchI pc story
        if count >= maxSteps then I_Stop else
          I_Trace count pc ins (k s { pc = pc', count = count + 1 } ins)

      FetchHeader{} -> do
        let State{story,pc} = s
        let (rh,pc') = fetchH pc story
        k s { pc = pc' } rh

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
        let _debug = id --I_Debug (show ("SetLocal",n,v))
        _debug $ k s { locals = Map.insert n v locals } ()

      EqualAny vs -> do
        let
          res =
            case vs of
              [v1,v2] -> v1==v2
              [v1,v2,v3] -> v1==v2 || v1==v3
              vs -> error (show ("EqualAny",vs))
        let _debug = id --I_Debug (show ("EqualAny",vs,res))
        _debug $ k s res
      IsZero value -> do
        let res = (value == 0)
        let _debug = id --I_Debug (show ("IsZero",value,res))
        _debug $ k s res
      BinOp bin v1 v2 -> do
        let res = case bin of
              BAdd -> v1 + v2
              BSub -> v1 - v2
              BAnd -> v1 .&. v2
        let _debug = id --I_Debug (show ("BinOp",bin,v1,v2,res))
        _debug $ k s res

      GetByte a -> do
        let State{story,overrides} = s
        let (_over,b) =
              case Map.lookup a overrides of
                Just b -> (True,b)
                Nothing -> (False,readStoryByte story a)
        let _debug = id --I_Debug (show ("GetByte",a,_over,b))
        _debug $ k s b

      SetByte a b -> do
        let State{overrides} = s
        let _debug = id --I_Debug (show ("SetByte",a,b))
        _debug $ k s { overrides = Map.insert a b overrides } ()

      PushStack v -> do
        let State{stack} = s
        k s { stack = v : stack } ()

      PopStack -> do
        let State{stack} = s
        case stack of
          [] -> error "PopStack: []"
          v:stack -> do
            k s { stack } v

fetchI :: Addr -> Story -> (Instruction,Addr) --TODO: inline
fetchI a story =
  case (runFetch a story fetchInstruction) of
    Left e -> error (show ("fetchI",e))
    Right x -> x

fetchH :: Addr -> Story -> (RoutineHeader,Addr) --TODO: inline
fetchH a story =
  case (runFetch a story fetchRoutineHeader) of
    Left e -> error (show ("fetchH",e))
    Right x -> x


--[interpreter state]-------------------------------------------------

data State = State
  { story :: Story
  , pc :: Addr
  , count :: Int
  , stack :: [Value]
  , locals :: Map Byte Value
  , frames :: [Frame]
  , overrides :: Map Addr Byte
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
