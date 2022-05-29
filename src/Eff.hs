
-- | The computation effect of z-machine execution.
module Eff (Eff(..)) where

import Control.Monad (ap,liftM)
import Dictionary (Dict)
import Header (Header)
import Numbers (Addr,Byte,Value)
import Operation (Operation,Target,RoutineHeader)

instance Functor (Eff a b s v) where fmap = liftM
instance Applicative (Eff a b s v) where pure = return; (<*>) = ap
instance Monad (Eff a b s v) where return = Ret; (>>=) = Bind

-- TODO: introduce a Phase typeclass

data Eff a b s v x where
  Ret :: x -> Eff a b s v x
  Bind :: Eff a b s v x -> (x -> Eff a b s v y) -> Eff a b s v y
  Debug :: Show x => x -> Eff a b s v ()

  StoryHeader :: Eff a b s v Header
  TheDictionary :: Eff a b s v Dict

  GamePrint :: s -> Eff a b s v ()
  ReadInputFromUser :: (s,v,v) -> Eff a b s v s
  GetText :: a -> Eff a b s v s
  FetchI :: Eff a b s v Operation
  FetchRoutineHeader :: Eff a b s v RoutineHeader

  PushFrame :: a -> Target -> Eff a b s v ()
  PopFrame :: Eff a b s v Target

  GetPC :: Eff a b s v a
  SetPC :: a -> Eff a b s v ()
  GetLocal :: b -> Eff a b s v v
  SetLocal :: b -> v -> Eff a b s v ()
  EqualAny :: [v] -> Eff a b s v Bool
  IsZero :: v -> Eff a b s v Bool
  GetByte :: a -> Eff a b s v b
  SetByte :: a -> b -> Eff a b s v ()
  PushStack :: v -> Eff a b s v ()
  PopStack :: Eff a b s v v
  Random :: v -> Eff a b s v v
  Quit :: Eff a b s v ()

  -- TODO: Can't return a concrete list, because the length is data-dependent.
  StringBytes :: s -> Eff a b s v [b]
  Tokenize :: s -> Eff a b s v ([(b,s)],s)

  LookupInStrings :: [s] -> s -> Eff a b s v (Maybe Int)

  -- TODO: capture Pure ops in a sep type, or maybe in the Phase typeclase
  Add :: v -> v -> Eff a b s v v
  Address :: v -> Eff a b s v a
  And :: v -> v -> Eff a b s v v
  BwAnd :: b -> b -> Eff a b s v b
  ClearBit :: b -> b -> Eff a b s v b
  DeAddress :: a -> Eff a b s v v
  Div :: v -> v -> Eff a b s v v
  Div8 :: v -> Eff a b s v v
  GreaterThan :: v -> v -> Eff a b s v Bool
  GreaterThanEqual :: v -> v -> Eff a b s v Bool
  HiByte :: v -> Eff a b s v b
  IsZeroAddress :: a -> Eff a b s v Bool
  IsZeroByte :: b -> Eff a b s v Bool
  LessThan :: v -> v -> Eff a b s v Bool
  LessThanByte :: b -> b -> Eff a b s v Bool
  LessThanEqual :: v -> v -> Eff a b s v Bool
  ListLength :: [x] -> Eff a b s v b
  LitA :: Addr -> Eff a b s v a
  LitB :: Byte -> Eff a b s v b
  LitS :: String -> Eff a b s v s
  LitV :: Value -> Eff a b s v v
  LoByte :: v -> Eff a b s v b
  MakeWord :: b -> b -> Eff a b s v v
  MinusByte :: b -> b -> Eff a b s v b
  Mod8 :: v -> Eff a b s v b
  Mul :: v -> v -> Eff a b s v v
  Offset :: a -> v -> Eff a b s v a
  PackedAddress :: v -> Eff a b s v a
  SetBit :: b -> b -> Eff a b s v b
  SevenMinus :: b -> Eff a b s v b
  ShiftR :: b -> Int -> Eff a b s v b
  ShowNumber :: v -> Eff a b s v s
  SingleChar :: v -> Eff a b s v s
  StringLength :: s -> Eff a b s v b
  Sub :: v -> v -> Eff a b s v v
  TestBit :: b -> b -> Eff a b s v Bool
  Widen :: b -> Eff a b s v v
