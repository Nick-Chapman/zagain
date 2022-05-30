
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
  type Text p
  type Value p
  type Vector p :: * -> *

data Eff p x where
  Ret :: x -> Eff p x
  Bind :: Eff p x -> (x -> Eff p y) -> Eff p y
  Debug :: Show x => x -> Eff p ()

  StoryHeader :: Eff p Header
  TheDictionary :: Eff p Dict

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
  EqualAny :: [Value p] -> Eff p Bool
  IsZero :: Value p -> Eff p Bool
  GetByte :: Addr p -> Eff p (Byte p)
  SetByte :: Addr p -> Byte p -> Eff p ()
  PushStack :: Value p -> Eff p ()
  PopStack :: Eff p (Value p)
  Random :: Value p -> Eff p (Value p)
  Quit :: Eff p ()

  StringBytes :: Text p -> Eff p (Vector p (Byte p))
  Tokenize :: Text p -> Eff p (Vector p (Byte p,Text p),Text p)
  ListLength :: Vector p x -> Eff p (Byte p)
  Foreach :: Vector p x -> (Int -> x -> Eff p ()) -> Eff p ()

  -- TODO: LookupInStrings takes a constant known dict, and so we dont need (Text p)
  LookupInStrings :: [Text p] -> Text p -> Eff p (Maybe Int)

  -- TODO: capture Pure ops in a sep type, or maybe in the Phase typeclase
  Add :: Value p -> Value p -> Eff p (Value p)
  Address :: Value p -> Eff p (Addr p)
  And :: Value p -> Value p -> Eff p (Value p)
  BwAnd :: Byte p -> Byte p -> Eff p (Byte p)
  ClearBit :: Byte p -> Byte p -> Eff p (Byte p)
  DeAddress :: Addr p -> Eff p (Value p)
  Div :: Value p -> Value p -> Eff p (Value p)
  Div8 :: Value p -> Eff p (Value p)
  GreaterThan :: Value p -> Value p -> Eff p Bool
  GreaterThanEqual :: Value p -> Value p -> Eff p Bool
  HiByte :: Value p -> Eff p (Byte p)
  IsZeroAddress :: Addr p -> Eff p Bool
  IsZeroByte :: Byte p -> Eff p Bool
  LessThan :: Value p -> Value p -> Eff p Bool
  LessThanByte :: Byte p -> Byte p -> Eff p Bool
  LessThanEqual :: Value p -> Value p -> Eff p Bool
  LitA :: Numbers.Addr -> Eff p (Addr p)
  LitB :: Numbers.Byte -> Eff p (Byte p)
  LitS :: String -> Eff p (Text p)
  LitV :: Numbers.Value -> Eff p (Value p)
  LoByte :: Value p -> Eff p (Byte p)
  MakeWord :: Byte p -> Byte p -> Eff p (Value p)
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
  TestBit :: Byte p -> Byte p -> Eff p Bool
  Widen :: Byte p -> Eff p (Value p)
