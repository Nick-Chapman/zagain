
-- | The computation effect of z-machine execution.
module Eff (Eff(..),Phase(..),Control(..),StatusInfo(..)) where

import Control.Monad (ap,liftM)
import Header (Header)
import Numbers (Style)
import Operation (Operation,RoutineHeader)
import qualified Numbers (Addr,Byte,Value)

instance Functor (Eff p) where fmap = liftM
instance Applicative (Eff p) where pure = return; (<*>) = ap
instance Monad (Eff p) where return = Ret; (>>=) = Bind

class
  ( Show (Addr p)
  , Show (Byte p)
  , Show (Value p)
  , Show (Text p)
  ) => Phase p where
  type Addr p
  type Byte p
  type Pred p
  type Text p
  type Value p
  type Vector p a
  type Code p

data Control p
  = AtRoutineHeader { routine :: Addr p, numActuals :: Byte p } -- TODO: why phase-poly numActuals?
  | AtInstruction { pc :: Addr p }
  | AtReturnFromCall { caller :: Addr p, result :: Value p }

deriving instance Phase p => Show (Control p)

data StatusInfo text value = StatusInfo
  { room :: text
  , score :: value
  , turns :: value
  } deriving Show

data Eff p x where
  Ret :: x -> Eff p x
  Bind :: Eff p x -> (x -> Eff p y) -> Eff p y

  GamePrint :: Text p -> Eff p ()
  TextStyle :: (Style,Bool) -> Eff p ()

  Error :: String -> Eff p a -- runtime error
  Debug :: Show x => x -> Eff p () -- runtime debug
  Note :: Show x => x -> Eff p () -- make this appear in compiled code

  StoryHeader :: Eff p Header
  LookupInDict :: Text p -> Eff p (Addr p)

  ReadInputFromUser :: Maybe (StatusInfo (Text p) (Value p)) -> Eff p (Text p)
  GetText :: Addr p -> Eff p (Text p)

  FetchRoutineHeader :: Addr p -> Eff p (RoutineHeader, Addr p)
  FetchOperation :: Addr p -> Eff p (Operation, Addr p)

  TraceOperation :: Addr p -> Operation -> Eff p ()
  TraceRoutineCall :: Addr p -> Eff p ()

  GetControl :: Eff p (Control p)
  SetControl :: Control p -> Eff p ()

  MakeRoutineFrame :: Int -> Eff p ()
  PushFrame :: Addr p -> Byte p -> Eff p ()
  PopFrame :: Eff p (Addr p)

  GetNumActuals :: Eff p (Byte p)

  GetLocal :: Byte p -> Eff p (Value p)
  SetLocal :: Byte p -> Value p -> Eff p ()
  GetByte :: Addr p -> Eff p (Byte p)
  SetByte :: Addr p -> Byte p -> Eff p ()
  PushStack :: Value p -> Eff p ()
  PopStack :: Eff p (Value p)
  Random :: Value p -> Eff p (Value p)
  Quit :: Eff p ()

  IteString :: Pred p -> (Text p) -> (Text p) -> Eff p (Text p)

  If :: Pred p -> Eff p Bool
  Isolate :: Eff p () -> Eff p ()

  Fixpoint
    :: Value p
    -> ((Value p -> Eff p (Code p)) -> (Value p -> Eff p ()))
    -> Eff p ()

  Link :: Code p -> Eff p ()

  IndexVecB :: Vector p (Byte p) -> Value p -> Eff p (Byte p)
  IndexVecT :: Vector p (Text p) -> Value p -> Eff p (Text p)

  LitA :: Numbers.Addr -> Eff p (Addr p)
  LitB :: Numbers.Byte -> Eff p (Byte p)
  LitS :: String -> Eff p (Text p)
  LitV :: Numbers.Value -> Eff p (Value p)

  Add :: Value p -> Value p -> Eff p (Value p)
  Address :: Value p -> Eff p (Addr p)
  And :: Value p -> Value p -> Eff p (Value p)
  BwAnd :: Byte p -> Byte p -> Eff p (Byte p)
  ClearBit :: Byte p -> Byte p -> Eff p (Byte p)
  DeAddress :: Addr p -> Eff p (Value p)
  Div :: Value p -> Value p -> Eff p (Value p)
  Div8 :: Value p -> Eff p (Value p)
  Equal :: Value p -> Value p -> Eff p (Pred p)
  GreaterThan :: Value p -> Value p -> Eff p (Pred p)
  GreaterThanEqual :: Value p -> Value p -> Eff p (Pred p)
  HiByte :: Value p -> Eff p (Byte p)
  IsZero :: Value p -> Eff p (Pred p)
  IsZeroAddress :: Addr p -> Eff p (Pred p)
  IsZeroByte :: Byte p -> Eff p (Pred p)
  LessThan :: Value p -> Value p -> Eff p (Pred p)
  LessThanByte :: Byte p -> Byte p -> Eff p (Pred p)
  LessThanEqual :: Value p -> Value p -> Eff p (Pred p)
  LoByte :: Value p -> Eff p (Byte p)
  LogOr :: Pred p -> Pred p -> Eff p (Pred p)
  MakeHiLo :: Byte p -> Byte p -> Eff p (Value p)
  MinusByte :: Byte p -> Byte p -> Eff p (Byte p)
  Mod :: Value p -> Value p -> Eff p (Value p)
  Mul :: Value p -> Value p -> Eff p (Value p)
  Not :: Pred p -> Eff p (Pred p)
  Offset :: Addr p -> Value p -> Eff p (Addr p)
  Or :: Value p -> Value p -> Eff p (Value p)
  PackedAddress :: Value p -> Eff p (Addr p)
  SetBit :: Byte p -> Byte p -> Eff p (Byte p)
  SevenMinus :: Byte p -> Eff p (Byte p)
  ShiftR :: Byte p -> Value p -> Eff p (Byte p)
  ShowNumber :: Value p -> Eff p (Text p)
  SingleChar :: Value p -> Eff p (Text p)
  StringBytes :: Text p -> Eff p (Vector p (Byte p))
  StringLength :: Text p -> Eff p (Byte p)
  Sub :: Value p -> Value p -> Eff p (Value p)
  TestBit :: Byte p -> Byte p -> Eff p (Pred p)
  Tokenize :: Text p -> Eff p (Byte p,Vector p (Byte p),Vector p (Text p))
  Widen :: Byte p -> Eff p (Value p)
