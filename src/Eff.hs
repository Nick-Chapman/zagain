
-- | The computation effect of z-machine execution.
module Eff (Eff(..)) where

import Control.Monad (ap,liftM)
import Dictionary (Dict)
import Header (Header)
import Numbers (Addr,Byte,Value)
import Operation (Operation,Target,RoutineHeader)

instance Functor (Eff vec a b s v) where fmap = liftM
instance Applicative (Eff vec a b s v) where pure = return; (<*>) = ap
instance Monad (Eff vec a b s v) where return = Ret; (>>=) = Bind

-- TODO: introduce a Phase typeclass

data Eff (vec :: * -> *) a b s v x where
  Ret :: x -> Eff vec a b s v x
  Bind :: Eff vec a b s v x -> (x -> Eff vec a b s v y) -> Eff vec a b s v y
  Debug :: Show x => x -> Eff vec a b s v ()

  StoryHeader :: Eff vec a b s v Header
  TheDictionary :: Eff vec a b s v Dict

  GamePrint :: s -> Eff vec a b s v ()
  ReadInputFromUser :: (s,v,v) -> Eff vec a b s v s
  GetText :: a -> Eff vec a b s v s
  FetchI :: Eff vec a b s v Operation
  FetchRoutineHeader :: Eff vec a b s v RoutineHeader

  PushFrame :: a -> Target -> Eff vec a b s v ()
  PopFrame :: Eff vec a b s v Target

  GetPC :: Eff vec a b s v a
  SetPC :: a -> Eff vec a b s v ()
  GetLocal :: b -> Eff vec a b s v v
  SetLocal :: b -> v -> Eff vec a b s v ()
  EqualAny :: [v] -> Eff vec a b s v Bool
  IsZero :: v -> Eff vec a b s v Bool
  GetByte :: a -> Eff vec a b s v b
  SetByte :: a -> b -> Eff vec a b s v ()
  PushStack :: v -> Eff vec a b s v ()
  PopStack :: Eff vec a b s v v
  Random :: v -> Eff vec a b s v v
  Quit :: Eff vec a b s v ()

  StringBytes :: s -> Eff vec a b s v (vec b)
  Tokenize :: s -> Eff vec a b s v (vec (b,s),s)
  ListLength :: vec x -> Eff vec a b s v b
  Foreach :: vec x -> (Int -> x -> Eff vec a b s v ()) -> Eff vec a b s v ()

  LookupInStrings :: [s] -> s -> Eff vec a b s v (Maybe Int)

  -- TODO: capture Pure ops in a sep type, or maybe in the Phase typeclase
  Add :: v -> v -> Eff vec a b s v v
  Address :: v -> Eff vec a b s v a
  And :: v -> v -> Eff vec a b s v v
  BwAnd :: b -> b -> Eff vec a b s v b
  ClearBit :: b -> b -> Eff vec a b s v b
  DeAddress :: a -> Eff vec a b s v v
  Div :: v -> v -> Eff vec a b s v v
  Div8 :: v -> Eff vec a b s v v
  GreaterThan :: v -> v -> Eff vec a b s v Bool
  GreaterThanEqual :: v -> v -> Eff vec a b s v Bool
  HiByte :: v -> Eff vec a b s v b
  IsZeroAddress :: a -> Eff vec a b s v Bool
  IsZeroByte :: b -> Eff vec a b s v Bool
  LessThan :: v -> v -> Eff vec a b s v Bool
  LessThanByte :: b -> b -> Eff vec a b s v Bool
  LessThanEqual :: v -> v -> Eff vec a b s v Bool
  LitA :: Addr -> Eff vec a b s v a
  LitB :: Byte -> Eff vec a b s v b
  LitS :: String -> Eff vec a b s v s
  LitV :: Value -> Eff vec a b s v v
  LoByte :: v -> Eff vec a b s v b
  MakeWord :: b -> b -> Eff vec a b s v v
  MinusByte :: b -> b -> Eff vec a b s v b
  Mod :: v -> v -> Eff vec a b s v v
  Mul :: v -> v -> Eff vec a b s v v
  Offset :: a -> v -> Eff vec a b s v a
  PackedAddress :: v -> Eff vec a b s v a
  SetBit :: b -> b -> Eff vec a b s v b
  SevenMinus :: b -> Eff vec a b s v b
  ShiftR :: b -> Int -> Eff vec a b s v b
  ShowNumber :: v -> Eff vec a b s v s
  SingleChar :: v -> Eff vec a b s v s
  StringLength :: s -> Eff vec a b s v b
  Sub :: v -> v -> Eff vec a b s v v
  TestBit :: b -> b -> Eff vec a b s v Bool
  Widen :: b -> Eff vec a b s v v
