
-- | The computation effect of z-machine execution.
module Eff (Eff(..),Phase(..)) where

import Control.Monad (ap,liftM)
import Dictionary (Dict)
import Header (Header)
import qualified Numbers (Addr,Byte,Value)
import Operation (Operation,Target,RoutineHeader)

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind

class Phase p where
  type Addr p
  type Byte p
  type Pred p
  type Text p
  type Value p
  type Vector p :: * -> *

data Eff p x where
  Ret :: x -> Eff p x
  Bind :: Eff p x -> (x -> Eff p y) -> Eff p y
  Debug :: Show x => x -> Eff p ()

  StoryHeader :: Eff p Header
  TheDictionary :: Eff p Dict
  LookupInStrings :: [String] -> Text p -> Eff p (Maybe Int)

  GamePrint :: Text p -> Eff p ()
  ReadInputFromUser :: (Text p,Value p,Value p) -> Eff p (Text p)
  GetText :: Addr p -> Eff p (Text p)
  FetchI :: Eff p Operation
  FetchRoutineHeader :: Eff p RoutineHeader

  PushFrame :: Addr p -> Target -> Eff p ()
  PopFrame :: Eff p Target

  GetPC :: Eff p (Addr p)
  SetPC :: Addr p -> Eff p ()
  GetLocal :: Byte p -> Eff p (Value p)
  SetLocal :: Byte p -> Value p -> Eff p ()
  EqualAny :: [Value p] -> Eff p (Pred p)
  IsZero :: Value p -> Eff p (Pred p)

  GetByte :: Addr p -> Eff p (Byte p)
  SetByte :: Addr p -> Byte p -> Eff p ()
  PushStack :: Value p -> Eff p ()
  PopStack :: Eff p (Value p)
  Random :: Value p -> Eff p (Value p)
  Quit :: Eff p ()
  If :: Pred p -> Eff p Bool

  StringBytes :: Text p -> Eff p (Vector p (Byte p))
  Tokenize :: Text p -> Eff p (Vector p (Byte p,Text p),Text p)
  ListLength :: Vector p x -> Eff p (Byte p)
  Foreach :: Vector p x -> (Int -> x -> Eff p ()) -> Eff p ()

  Add :: Value p -> Value p -> Eff p (Value p)
  Address :: Value p -> Eff p (Addr p)
  And :: Value p -> Value p -> Eff p (Value p)
  BwAnd :: Byte p -> Byte p -> Eff p (Byte p)
  ClearBit :: Byte p -> Byte p -> Eff p (Byte p)
  DeAddress :: Addr p -> Eff p (Value p)
  Div :: Value p -> Value p -> Eff p (Value p)
  Div8 :: Value p -> Eff p (Value p)
  GreaterThan :: Value p -> Value p -> Eff p (Pred p)
  GreaterThanEqual :: Value p -> Value p -> Eff p (Pred p)
  HiByte :: Value p -> Eff p (Byte p)
  IsZeroAddress :: Addr p -> Eff p (Pred p)
  IsZeroByte :: Byte p -> Eff p (Pred p)
  LessThan :: Value p -> Value p -> Eff p (Pred p)
  LessThanByte :: Byte p -> Byte p -> Eff p (Pred p)
  LessThanEqual :: Value p -> Value p -> Eff p (Pred p)
  LitA :: Numbers.Addr -> Eff p (Addr p)
  LitB :: Numbers.Byte -> Eff p (Byte p)
  LitS :: String -> Eff p (Text p)
  LitV :: Numbers.Value -> Eff p (Value p)
  LoByte :: Value p -> Eff p (Byte p)
  MakeHiLo :: Byte p -> Byte p -> Eff p (Value p)
  MinusByte :: Byte p -> Byte p -> Eff p (Byte p)
  Mod :: Value p -> Value p -> Eff p (Value p)
  Mul :: Value p -> Value p -> Eff p (Value p)
  Offset :: Addr p -> Value p -> Eff p (Addr p)
  PackedAddress :: Value p -> Eff p (Addr p)
  SetBit :: Byte p -> Byte p -> Eff p (Byte p)
  SevenMinus :: Byte p -> Eff p (Byte p)
  ShiftR :: Byte p -> Int -> Eff p (Byte p)
  ShowNumber :: Value p -> Eff p (Text p)
  SingleChar :: Value p -> Eff p (Text p)
  StringLength :: Text p -> Eff p (Byte p)
  Sub :: Value p -> Value p -> Eff p (Value p)
  TestBit :: Byte p -> Byte p -> Eff p (Pred p)
  Widen :: Byte p -> Eff p (Value p)
