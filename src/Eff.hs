
-- | The computation effect of z-machine execution.
module Eff (Eff(..),Bin(..)) where

import Control.Monad (ap,liftM)
import Dictionary (Dict)
import Header (Header)
import Numbers (Addr,Byte,Value)
import Operation (Operation,Target,RoutineHeader)

instance Functor (Eff b v) where fmap = liftM
instance Applicative (Eff b v) where pure = return; (<*>) = ap
instance Monad (Eff b v) where return = Ret; (>>=) = Bind

-- TODO: Bool -> p
-- TODO: Addr -> a
-- TODO: String -> s
-- TODO: String ops: read at Addr; conv-from-value; (short-name)
data Eff b v x where
  Ret :: x -> Eff b v x
  Bind :: Eff b v x -> (x -> Eff b v y) -> Eff b v y
  Debug :: Show x => x -> Eff b v ()
  GamePrint :: String -> Eff b v ()
  ReadInputFromUser :: (String,String) -> Eff b v String
  GetText :: Addr -> Eff b v String
  FetchI :: Eff b v Operation
  FetchRoutineHeader :: Eff b v RoutineHeader -- TODO: take PC?
  FetchDict :: Eff b v Dict -- TODO: no Fetch here! (Fetch mean PC rel)

  PushFrame :: Addr -> Target -> Eff b v ()
  PopFrame :: Eff b v Target -- TODO: avoid Eff representation of Target

  -- TODO: replace PushFrame/PopFrame with Call/Return
  -- Call :: Addr -> Eff b v v
  -- Return :: v -> Eff b v ()

  GetPC :: Eff b v Addr
  SetPC :: Addr -> Eff b v ()
  GetLocal :: b -> Eff b v v
  SetLocal :: b -> v -> Eff b v ()
  EqualAny :: [v] -> Eff b v Bool
  IsZero :: v -> Eff b v Bool
  BinOp :: Bin -> v -> v -> Eff b v v
  GetByte :: Addr -> Eff b v b
  SetByte :: Addr -> b -> Eff b v ()
  PushStack :: v -> Eff b v ()
  PopStack :: Eff b v v
  Random :: v -> Eff b v v
  Quit :: Eff b v ()
  StoryHeader :: Eff b v Header

  Div8 :: v -> Eff b v v
  Mod8 :: v -> Eff b v b
  SevenMinus :: b -> Eff b v b
  ClearBit :: b -> b -> Eff b v b
  SetBit :: b -> b -> Eff b v b
  TestBit :: b -> b -> Eff b v Bool

  MakeWord :: b -> b -> Eff b v v
  Widen :: b -> Eff b v v
  LoByte :: v -> Eff b v b
  HiByte :: v -> Eff b v b

  LitB :: Byte -> Eff b v b
  LitV :: Value -> Eff b v v
  ShiftR :: b -> Int -> Eff b v b
  BwAnd :: b -> b -> Eff b v b

  IsZeroByte :: b -> Eff b v Bool

data Bin = BAdd | BSub | BMul | BDiv | BAnd
  deriving Show
