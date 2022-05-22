
-- | Interpreter for z-machine effects.
module Interpreter (State,run) where

import Action (Action)
import Data.Bits ((.&.),shiftL)
import Data.Map (Map)
import Decode (fetchOperation,fetchRoutineHeader,ztext)
import Dictionary (fetchDict)
import Eff (Eff(..),Bin(..))
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Byte,Addr,Value)
import Operation (Target)
import Story (Story(header),readStoryByte)
import Text.Printf (printf)
import qualified Action as A (Action(..))
import qualified Data.Map as Map

--[interpreter for execution effects]----------------------------------

run :: Word -> Story -> Eff () -> Action
run seed story e0 = loop (initState seed pc0) e0 k0
  where
    header@Header{initialPC=pc0} = Story.header story

    k0 State{count,lastCount} () = A.Stop (count-lastCount)

    (dict,_) = runFetch 0 story fetchDict

    loop :: State -> Eff a -> (State -> a -> Action) -> Action
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      GamePrint mes -> A.Output mes (k s ())
      Debug a -> A.Debug (show a) (k s ())

      ReadInputFromUser (p1,p2) -> do
        let State{count,lastCount} = s
        A.Input (p1,p2) (count-lastCount) $ \response -> k s { lastCount = count } response

      GetText a -> do
        let (text,_) = runFetch a story ztext
        k s text

      FetchI -> do -- TODO: share code common to all Fetch* ops
        let State{pc,count} = s
        let (ins,pc') = runFetch pc story fetchOperation
        let s' = s { pc = pc'
                   , count = count + 1
                   }
        A.TraceInstruction (show s) count pc ins (k s' ins)

      FetchRoutineHeader -> do -- TODO: share code common to all Fetch* ops
        let State{pc} = s
        let (rh,pc') = runFetch pc story fetchRoutineHeader
        k s { pc = pc' } rh

      FetchDict -> do
        k s dict

      PushFrame addr target -> do
        let State{pc,stack,locals,frames} = s
        k s { pc = addr
            , frames = Frame { pc, target, stack, locals } : frames
            , stack = []
            , locals = Map.empty
            } ()

      PopFrame -> do
        let State{frames} = s
        case frames of
          [] -> error "PopFrame, frames=[]"
          Frame{pc,target,stack,locals}:frames -> do
            k s { pc, stack, locals, frames } target

      GetPC -> let State{pc} = s in k s pc
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
              [v1,v2,v3,v4] -> v1==v2 || v1==v3 || v1==v4
              vs -> error (show ("EqualAny",vs))
        k s res

      IsZero value -> do
        let res = (value == 0)
        k s res

      BinOp bin v1 v2 -> do
        let res = case bin of
              BAdd -> v1 + v2
              BSub -> v1 - v2
              BMul -> v1 * v2
              BDiv -> v1 `div` v2
              BAnd -> v1 .&. v2
        k s res

      GetByte a -> do
        let State{overrides} = s
        let (_over,b) =
              case Map.lookup a overrides of
                Just b -> (True,b)
                Nothing -> (False,readStoryByte story a)
        --A.Debug (show ("GetByte",a,"->",b)) $
        k s b

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

      Random range -> do
        let State{seed} = s
        let x = stepRandom seed
        let result = x `mod` (range+1) + 1
        k s { seed = x } result

      Quit -> do
        -- dont call "k" but instead "k0"
        k0 s ()

      StoryHeader -> do
        k s header

--[interpreter state]-------------------------------------------------

data State = State
  { pc :: Addr
  , lastCount :: Int
  , count :: Int
  , stack :: [Value]
  , locals :: Map Byte Value
  , frames :: [Frame]
  , overrides :: Map Addr Byte
  , seed :: Word
  }

instance Show State where
  show State{pc,locals,stack} = printf "[%s] (%d) locals:%s, stack:#%d%s" (show pc) num x depth y
    where
      x = concat
        [ " " ++ printf "%05s" (show v)
        | i <- [1.. fromIntegral num]
        , let v::Value = maybe 11111 id $ Map.lookup i locals
        ]
      y = concat
        [ " " ++ printf "%05s" (show v)
        | v <- stack
        ]
      num::Int =
        fromIntegral $ maximum (0 : [ k | k <- Map.keys locals ])
      depth = length stack

initState :: Word -> Addr -> State
initState seed pc = do
  State { pc
        , lastCount = 0
        , count = 0
        , stack = []
        , locals = Map.empty
        , frames = []
        , overrides = Map.empty
        , seed
        }

-- pulled from wikipedia "Linear congruential generator"
stepRandom :: Word -> Word
stepRandom x = (x * a  + c) `mod` m
  where
    a = 1103515245
    c = 12345
    m = 1 `shiftL` 31

data Frame = Frame
  { pc :: Addr
  , target :: Target
  , stack :: [Value]
  , locals :: Map Byte Value
  }
  deriving Show
