
-- | Interpreter for z-machine effects.
module Interpreter (State,runEffect) where

import Action (Action)
import Data.Bits (shiftL)
import Data.Map (Map)
import Decode (fetchOperation,fetchRoutineHeader,ztext)
import Dictionary (fetchDict)
import Eff (Eff(..),Phase,PCmode(..),StatusInfo(..))
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Byte,Addr,Value)
import Operation (Target)
import Story (Story(header),readStoryByte,OOB_Mode(..))
import Text.Printf (printf)
import qualified Action as A (Action(..),StatusLine(..))
import qualified Data.Map as Map
import qualified Eff (Phase(..))

import qualified Primitive as Prim
import Primitive (evalP1,evalP2)

data Interpret

instance Phase Interpret where
  type Addr Interpret = Addr
  type Byte Interpret = Byte
  type Pred Interpret = Bool
  type Text Interpret = String
  type Value Interpret = Value
  type Vector Interpret a = [a]

type Effect x = Eff Interpret x

--[interpreter for execution effects]----------------------------------

runEffect :: Byte -> Word -> Story -> Effect () -> Action
runEffect screenWidth seed story smallStep = do
  loop (initState screenWidth seed pc0 AtInstruction) e0 k0
  where
    e0 = do smallStep; e0

    oob who = OOB_Error ("runEffect:"++who)

    header@Header{zv,initialPC=pc0} = Story.header story

    k0 State{count,lastCount} () = A.Stop (count-lastCount)

    (dict,_) = runFetch (oob "fetchDict") 0 story fetchDict

    loop :: forall loopType. State -> Effect loopType -> (State -> loopType -> Action) -> Action
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      GamePrint mes -> A.Output mes (k s ())
      Debug a -> A.Debug (show a) (k s ())

      Error s -> error ("runEffect: " ++ s)

      TheDictionary -> do
        k s dict

      StoryHeader -> do
        k s header

      ReadInputFromUser statusInfo -> do
        let State{count,lastCount} = s
        let
          statusLineM =
            case statusInfo of
              Nothing -> Nothing
              Just (StatusInfo{room,score,turns}) ->
                Just (A.StatusLine
                      { left = room
                      , right = printf "score:%s--turns:%s" (show score) (show turns)
                      })
        A.Input statusLineM (count-lastCount) $ \response -> k s { lastCount = count } response

      GetText a -> do
        let (text,_) = runFetch (oob "GetText") a story ztext
        k s text

      GetPCmode -> let State{pcMode} = s in k s pcMode
      SetPCmode pcMode -> k s { pcMode } ()

      FetchI -> do
        let State{pc,count} = s
        let (ins,pc') = runFetch (oob "FetchI") pc story fetchOperation
        A.TraceInstruction (show s) count pc ins $
          k s { pc = pc', count = count + 1 } ins

      FetchRoutineHeader -> do
        let State{pc} = s
        let (rh,pc') = runFetch (oob "FetchRoutineHeader") pc story fetchRoutineHeader
        k s { pc = pc' } rh

      PushFrame -> do
        let State{stack,locals,frames} = s
        k s { frames = Frame { stack, locals } : frames
            , stack = []
            , locals = Map.empty
            } ()

      PopFrame -> do
        let State{frames} = s
        case frames of
          [] -> error "PopFrame, frames=[]"
          Frame{stack,locals}:frames -> do
            k s { stack, locals, frames } ()

      PushCallStack addr target -> do
        let State{callstack} = s
        k s { callstack = (addr,target) : callstack } ()

      PopCallStack -> do
        let State{callstack} = s
        case callstack of
          [] -> error "PopCallStack[]"
          (pc,target):callstack -> do
            k s { callstack } (pc,target)

      GetPC -> let State{pc} = s in k s pc
      SetPC pc -> k s { pc } ()
      SetPC_forCall pc -> do
        A.TraceRoutineCall pc $ -- purely for Disassemble/explore-walkthrough
          k s { pc } ()

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

      -- pure unary primitives
      Address x -> prim1 x Prim.Address
      DeAddress x -> prim1 x Prim.DeAddress
      Div8 x -> prim1 x Prim.Div8
      EqualAny x -> prim1 x Prim.EqualAny
      HiByte x -> prim1 x Prim.HiByte
      IsZero x -> prim1 x Prim.IsZero
      IsZeroAddress x -> prim1 x Prim.IsZeroAddress
      IsZeroByte x -> prim1 x Prim.IsZeroByte
      LoByte x -> prim1 x Prim.LoByte
      PackedAddress x -> prim1 x (Prim.PackedAddress zv)
      SevenMinus x -> prim1 x Prim.SevenMinus
      ShowNumber x -> prim1 x Prim.ShowNumber
      SingleChar x -> prim1 x Prim.SingleChar
      StringBytes x -> prim1 x Prim.StringBytes
      StringLength x -> prim1 x Prim.StringLength
      Tokenize x -> prim1 x Prim.Tokenize
      Widen x -> prim1 x Prim.Widen

      -- pure binary primitives
      Add x y -> prim2 x y Prim.Add
      And x y -> prim2 x y Prim.And
      BwAnd x y -> prim2 x y Prim.BwAnd
      ClearBit x y -> prim2 x y Prim.ClearBit
      Div x y -> prim2 x y Prim.Div
      GreaterThan x y -> prim2 x y Prim.GreaterThan
      GreaterThanEqual x y -> prim2 x y Prim.GreaterThanEqual
      LessThan x y -> prim2 x y Prim.LessThan
      LessThanByte x y -> prim2 x y Prim.LessThanByte
      LessThanEqual x y -> prim2 x y Prim.LessThanEqual
      LookupInStrings x y -> prim2 x y Prim.LookupInStrings
      MakeHiLo x y -> prim2 x y Prim.MakeHiLo
      MinusByte x y -> prim2 x y Prim.MinusByte
      Mod x y -> prim2 x y Prim.Mod
      Mul x y -> prim2 x y Prim.Mul
      Offset x y -> prim2 x y Prim.Offset
      Or x y -> prim2 x y Prim.Or
      SetBit x y -> prim2 x y Prim.SetBit
      ShiftR x y -> prim2 x y Prim.ShiftR
      Sub x y -> prim2 x y Prim.Sub
      TestBit x y -> prim2 x y Prim.TestBit

      where
        prim1 :: forall x. x -> Prim.P1 x loopType -> Action
        prim1 x p1 = k s (evalP1 p1 x)

        prim2 :: forall x y. x -> y -> Prim.P2 x y loopType -> Action
        prim2 x y p2 = k s (evalP2 p2 x y)


--[interpreter state]-------------------------------------------------

data State = State
  { pc :: Addr
  , pcMode :: PCmode
  , lastCount :: Int
  , count :: Int
  , stack :: [Value]
  , locals :: Map Byte Value
  , frames :: [Frame]
  , callstack :: [(Addr,Target)]
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

initState :: Byte -> Word -> Addr -> PCmode -> State
initState screenWidth seed pc pcMode = do
  State { pc
        , pcMode
        , lastCount = 0
        , count = 0
        , stack = []
        , locals = Map.empty
        , frames = []
        , callstack = []
        , overrides = Map.fromList [(33,screenWidth)]
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
  { stack :: [Value]
  , locals :: Map Byte Value
  }
  deriving Show
