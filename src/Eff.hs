
-- | The computation effect of z-machine execution.
module Eff (Eff(..),Bin(..)) where

import Control.Monad (ap,liftM)
import Dictionary (Dict)
import Header (Header)
import Numbers (Addr,Byte,Value)
import Operation (Operation,Target,RoutineHeader)

instance Functor (Eff a b s v) where fmap = liftM
instance Applicative (Eff a b s v) where pure = return; (<*>) = ap
instance Monad (Eff a b s v) where return = Ret; (>>=) = Bind

-- TODO: introduce a Phase typeclass

-- TODO: Bool -> p ?
-- TODO: String ops: read at Addr; conv-from-value; (short-name)
data Eff a b s v x where
  Ret :: x -> Eff a b s v x
  Bind :: Eff a b s v x -> (x -> Eff a b s v y) -> Eff a b s v y
  Debug :: Show x => x -> Eff a b s v ()
  GamePrint :: s -> Eff a b s v ()
  ReadInputFromUser :: (s,v,v) -> Eff a b s v s
  GetText :: a -> Eff a b s v s
  FetchI :: Eff a b s v Operation
  FetchRoutineHeader :: Eff a b s v RoutineHeader -- TODO: take PC?
  FetchDict :: Eff a b s v Dict -- TODO: no Fetch here! (Fetch mean PC rel)

  PushFrame :: a -> Target -> Eff a b s v ()
  PopFrame :: Eff a b s v Target -- TODO: avoid Eff representation of Target

  -- TODO: replace PushFrame/PopFrame with Call/Return
  -- Call :: a -> Eff a b s v v
  -- Return :: v -> Eff a b s v ()

  GetPC :: Eff a b s v a
  SetPC :: a -> Eff a b s v ()
  GetLocal :: b -> Eff a b s v v
  SetLocal :: b -> v -> Eff a b s v ()
  EqualAny :: [v] -> Eff a b s v Bool
  IsZero :: v -> Eff a b s v Bool
  BinOp :: Bin -> v -> v -> Eff a b s v v
  GetByte :: a -> Eff a b s v b
  SetByte :: a -> b -> Eff a b s v ()
  PushStack :: v -> Eff a b s v ()
  PopStack :: Eff a b s v v
  Random :: v -> Eff a b s v v
  Quit :: Eff a b s v ()
  StoryHeader :: Eff a b s v Header

  -- TODO: capture Pure ops in a sep type, or maybe in the Phase typeclase
  Div8 :: v -> Eff a b s v v
  Mod8 :: v -> Eff a b s v b
  SevenMinus :: b -> Eff a b s v b
  ClearBit :: b -> b -> Eff a b s v b
  SetBit :: b -> b -> Eff a b s v b
  TestBit :: b -> b -> Eff a b s v Bool

  MakeWord :: b -> b -> Eff a b s v v
  Widen :: b -> Eff a b s v v
  LoByte :: v -> Eff a b s v b
  HiByte :: v -> Eff a b s v b

  LitB :: Byte -> Eff a b s v b
  LitV :: Value -> Eff a b s v v
  ShiftR :: b -> Int -> Eff a b s v b
  BwAnd :: b -> b -> Eff a b s v b
  BwAndV :: v -> v -> Eff a b s v v

  IsZeroByte :: b -> Eff a b s v Bool
  LessThanByte :: b -> b -> Eff a b s v Bool
  MinusByte :: b -> b -> Eff a b s v b

  LitA :: Addr -> Eff a b s v a
  Address :: v -> Eff a b s v a
  DeAddress :: a -> Eff a b s v v
  PackedAddress :: v -> Eff a b s v a
  Offset :: a -> v -> Eff a b s v a

  LessThan :: v -> v -> Eff a b s v Bool
  LessThanEqual :: v -> v -> Eff a b s v Bool
  GreaterThan :: v -> v -> Eff a b s v Bool
  GreaterThanEqual :: v -> v -> Eff a b s v Bool

  LitS :: String -> Eff a b s v s
  IsZeroAddress :: a -> Eff a b s v Bool

  -- TODO: Can't return a concrete list, because the length is data-dependent.
  Tokenize :: s -> Eff a b s v ([(b,s)],s)
  ListLength :: [x] -> Eff a b s v b

  -- TODO: maybe this is not quite the right behaviour to abstract into Eff
  LookupInStrings :: [s] -> s -> Eff a b s v (Maybe Int)

  StringLength :: s -> Eff a b s v b
  StringBytes :: s -> Eff a b s v [b]

  SingleChar :: v -> Eff a b s v s
  ShowNumber :: v -> Eff a b s v s

data Bin = BAdd | BSub | BMul | BDiv | BAnd
  deriving Show
