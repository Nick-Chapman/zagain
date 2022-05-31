
-- | Interpreter for z-machine effects.
module Interpreter (State,runEffect) where

import Action (Action)
import Data.Bits ((.&.),shiftL,clearBit,setBit,testBit,shiftR)
import Data.Map (Map)
import Decode (fetchOperation,fetchRoutineHeader,ztext)
import Dictionary (fetchDict)
import Eff (Eff(..),Phase)
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Byte,Addr,Value,addrOfPackedWord,makeHiLo,equalAny)
import Operation (Target)
import Story (Story(header),readStoryByte,OOB_Mode(..))
import Text.Printf (printf)
import qualified Action as A (Action(..))
import qualified Data.Char as Char (ord,chr)
import qualified Data.Map as Map
import qualified Eff (Phase(..))
import qualified Lex (tokenize,lookupInStrings)

data Interpret

instance Phase Interpret where
  type Addr Interpret = Addr
  type Byte Interpret = Byte
  type Pred Interpret = Bool
  type Text Interpret = String
  type Value Interpret = Value
  type Vector Interpret = []

type Effect x = Eff Interpret x

--[interpreter for execution effects]----------------------------------

runEffect :: Word -> Story -> Effect () -> Action
runEffect seed story e0 = loop (initState seed pc0) e0 k0
  where
    oob who = OOB_Error ("runEffect:"++who)

    header@Header{initialPC=pc0} = Story.header story

    k0 State{count,lastCount} () = A.Stop (count-lastCount)

    (dict,_) = runFetch (oob "fetchDict") 0 story fetchDict

    loop :: State -> Effect a -> (State -> a -> Action) -> Action
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      GamePrint mes -> A.Output mes (k s ())
      Debug a -> A.Debug (show a) (k s ())

      TheDictionary -> do
        k s dict

      StoryHeader -> do
        k s header

      ReadInputFromUser (p1,score,turns) -> do
        let p2 = printf "score:%s--turns:%s" (show score) (show turns)
        let State{count,lastCount} = s
        A.Input (p1,p2) (count-lastCount) $ \response -> k s { lastCount = count } response

      GetText a -> do
        let (text,_) = runFetch (oob "GetText") a story ztext
        k s text

      FetchI -> do -- TODO: share code common to all Fetch* ops
        let State{pc,count} = s
        let (ins,pc') = runFetch (oob "FetchI") pc story fetchOperation
        let s' = s { pc = pc'
                   , count = count + 1
                   }
        A.TraceInstruction (show s) count pc ins (k s' ins)

      FetchRoutineHeader -> do -- TODO: share code common to all Fetch* ops
        let State{pc} = s
        let (rh,pc') = runFetch (oob "FetchRoutineHeader") pc story fetchRoutineHeader
        k s { pc = pc' } rh

      PushFrame addr target -> do
        let State{pc,stack,locals,frames} = s
        A.TraceRoutineCall addr $
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

      GetByte a -> do
        let State{overrides} = s
        let (_over,b) =
              case Map.lookup a overrides of
                Just b -> (True,b)
                Nothing -> (False,readStoryByte (oob "GetByte") story a)
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
        let result = x `mod` (fromIntegral range) + 1
        k s { seed = x } (fromIntegral result)

      Quit -> do
        -- dont call "k" but instead "k0"
        k0 s ()

      If pred -> k s pred

      Foreach xs f -> do
        loop s (sequence_ [ f i x | (i,x) <- zip [0..] xs ]) k

      LitA a -> k s a
      LitB b -> k s b
      LitS x -> k s x
      LitV v -> k s v

      Add v1 v2 -> k s (v1 + v2)
      Address v -> k s (fromIntegral v)
      And v1 v2 -> k s (v1 .&. v2)
      BwAnd b1 b2 -> k s (b1 .&. b2)
      ClearBit b n -> k s (b `clearBit` fromIntegral n)
      DeAddress a -> k s (fromIntegral a)
      Div v1 v2 -> k s (v1 `div` v2)
      Div8 v -> k s (v `div` 8)
      EqualAny vs -> k s (equalAny vs)
      GreaterThan v1 v2 -> k s (v1 > v2)
      GreaterThanEqual v1 v2 -> k s (v1 >= v2)
      HiByte v -> k s (fromIntegral (v `shiftR` 8))
      IsZero value -> k s (value == 0)
      IsZeroAddress a -> k s (a == 0)
      IsZeroByte b -> k s (b == 0)
      LessThan v1 v2 -> k s (v1 < v2)
      LessThanByte b1 b2 -> k s (b1 < b2)
      LessThanEqual v1 v2 -> k s (v1 <= v2)
      ListLength xs -> k s (fromIntegral $ length xs)
      LoByte v -> k s (fromIntegral (v .&. 0xff))
      LookupInStrings strings word -> k s (Lex.lookupInStrings strings word)
      MakeHiLo hi lo -> k s (makeHiLo hi lo)
      MinusByte b1 b2 -> k s (b1 - b2)
      Mod v1 v2 -> k s (v1 `mod` v2)
      Mul v1 v2 -> k s (v1 * v2)
      Offset base off -> k s (base + fromIntegral off)
      PackedAddress v -> k s (addrOfPackedWord v)
      SetBit b n -> k s (b `setBit` fromIntegral n)
      SevenMinus v -> k s (7-v)
      ShiftR b n -> k s (b `shiftR` n)
      ShowNumber v -> k s (show v)
      SingleChar v -> k s [Char.chr (fromIntegral v)]
      StringBytes str -> k s [ fromIntegral (Char.ord c) | c <- str ]
      StringLength str -> k s (fromIntegral $ length str)
      Sub v1 v2 -> k s (v1 - v2)
      TestBit b n -> k s (b `testBit` fromIntegral n)
      Tokenize str -> k s (Lex.tokenize str)
      Widen lo -> k s (fromIntegral lo)


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
