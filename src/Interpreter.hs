
-- | Interpreter for z-machine effects.
module Interpreter (State,run) where

import Action (Action,Stats(..))
import Data.Bits ((.&.))
import Data.Map (Map)
import Decode (fetchOperation,fetchRoutineHeader,ztext)
import Dictionary (fetchDict)
import Eff (Eff(..),Bin(..))
import Fetch (runFetch)
import Numbers (Byte,Addr,Value)
import Operation (Target)
import Story (Story,readStoryByte)
import Text.Printf (printf)
import qualified Action as A (Action(..))
import qualified Data.Map as Map

--[interpreter for execution effects]----------------------------------

run :: Story -> Eff () -> Action
run story e0 = loop (initState pc0) e0 k0
  where
    pc0 :: Addr = fromIntegral (readStoryWord story 0x6)

    k0 State{count,lastCount} () = A.Stop (count-lastCount)

    loop :: State -> Eff a -> (State -> a -> Action) -> Action
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      GamePrint mes -> A.Output mes (k s ())
      Debug a -> A.Debug (show a) (k s ())

      ReadInputFromUser -> do
        let State{count,lastCount} = s
        A.Input (count-lastCount) $ \response -> k s { lastCount = count } response

      GetText a -> do
        let State{stats} = s
        let (text,_,readCount) = runFetch a story ztext
        let Stats{ct} = stats
        let s' = s { stats = stats { ct = ct + readCount }}
        k s' text

      FetchI -> do -- TODO: share code common to all Fetch* ops
        let State{pc,count,stats} = s
        let (ins,pc',readCount) = runFetch pc story fetchOperation
        let Stats{ct} = stats
        let s' = s { pc = pc'
                   , count = count + 1
                   , stats = stats { ct = ct + readCount }
                   }
        A.Trace (show s) stats count pc ins (k s' ins)

      FetchHeader -> do -- TODO: share code common to all Fetch* ops
        let State{pc,stats} = s
        let (rh,pc',readCount) = runFetch pc story fetchRoutineHeader
        let Stats{ct} = stats
        let s' = s { pc = pc', stats = stats { ct = ct + readCount} }
        k s' rh

      FetchDict -> do -- TODO: fetch dict only once!
        let State{pc,stats} = s
        let (dict,pc',readCount) = runFetch pc story fetchDict
        let Stats{ct} = stats
        let s' = s { pc = pc', stats = stats { ct = ct + readCount} }
        k s' dict

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
        let State{overrides,stats} = s
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

      Random range -> do
        let State{oracle} = s
        case oracle of
          [] -> error "random oracle has run out!"
          (x,result):oracle -> if
            | (range /= x) -> error (show ("unexpected random range",range,x))
            | otherwise -> k s { oracle } result

      Quit -> do
        -- dont call "k" but instead "k0"
        k0 s ()

--[interpreter state]-------------------------------------------------

data State = State
  { pc :: Addr
  , lastCount :: Int
  , count :: Int
  , stack :: [Value]
  , locals :: Map Byte Value
  , frames :: [Frame]
  , overrides :: Map Addr Byte
  , stats :: Stats
  , oracle :: [(Int,Int)]
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

initState :: Addr -> State
initState pc = do
  State { pc
        , lastCount = 0
        , count = 0
        , stack = []
        , locals = Map.empty
        , frames = []
        , overrides = Map.empty
        , stats = Stats { ct = 0, rt = 0 }
        , oracle = oracle0
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

----------------------------------------------------------------------
--[random oracle to match mojozork (useful during dev)]---------------

oracle0 :: [(Int,Int)]
oracle0 =
  [ (100, 54), (100, 90), (100, 71), (100, 69), (100, 56)
  , (9, 4), (100, 40), (5, 1), (9, 8), (3, 2), (100, 70), (100, 77), (100, 70)
  , (100, 15), (100, 89), (100, 9), (100, 22), (100, 74), (100, 69), (100, 38), (100, 6)
  , (100, 85), (100, 25), (100, 4), (100, 41), (100, 84), (100, 15), (100, 6), (100, 76)
  , (100, 8), (100, 35), (100, 30), (100, 98), (100, 7), (100, 1), (100, 55), (100, 57)
  , (100, 41), (100, 61), (100, 53), (100, 32), (100, 32), (100, 31), (100, 3), (100, 48)
  , (100, 21), (100, 12), (100, 70), (100, 95), (100, 81), (100, 9), (100, 1), (100, 66)
  , (100, 35), (100, 6), (100, 7), (100, 20), (100, 21), (100, 14), (100, 96), (100, 30)
  , (100, 49), (100, 27), (100, 28), (100, 57), (100, 28), (100, 84), (100, 15), (100, 69)
  , (100, 46), (100, 68), (100, 1), (100, 78), (100, 1), (100, 4), (100, 27), (9, 1)
  , (4, 1), (9, 8), (4, 1), (9, 8), (4, 1), (9, 1), (6, 3), (9, 3)
  , (4, 1), (9, 6), (4, 2), (9, 4), (4, 3), (9, 5), (4, 3), (9, 3)
  , (4, 3), (9, 1), (6, 5), (9, 1), (4, 3), (9, 1), (6, 5), (9, 3)
  , (4, 3), (9, 8), (3, 1), (100, 79), (100, 94), (100, 43), (100, 1), (100, 11)
  ]
