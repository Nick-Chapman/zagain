
-- | The computation effect of z-machine execution.
module Eff (Eff(..),Bin(..)) where

import Control.Monad (ap,liftM)
import Dictionary (Dict)
import Header (Header)
import Numbers (Byte,Addr)
import Operation (Operation,Target,RoutineHeader)

instance Functor (Eff v) where fmap = liftM
instance Applicative (Eff v) where pure = return; (<*>) = ap
instance Monad (Eff v) where return = Ret; (>>=) = Bind

data Eff v x where
  Ret :: x -> Eff v x
  Bind :: Eff v x -> (x -> Eff v y) -> Eff v y
  Debug :: Show x => x -> Eff v ()
  GamePrint :: String -> Eff v ()
  ReadInputFromUser :: (String,String) -> Eff v String
  GetText :: Addr -> Eff v String
  FetchI :: Eff v Operation
  FetchRoutineHeader :: Eff v RoutineHeader
  FetchDict :: Eff v Dict
  PushFrame :: Addr -> Target -> Eff v ()
  PopFrame :: Eff v Target
  GetPC :: Eff v Addr
  SetPC :: Addr -> Eff v ()
  GetLocal :: Byte -> Eff v v
  SetLocal :: Byte -> v -> Eff v ()
  EqualAny :: [v] -> Eff v Bool
  IsZero :: v -> Eff v Bool
  BinOp :: Bin -> v -> v -> Eff v v
  GetByte :: Addr -> Eff v Byte
  SetByte :: Addr -> Byte -> Eff v ()
  PushStack :: v -> Eff v ()
  PopStack :: Eff v v
  Random :: v -> Eff v v
  Quit :: Eff v ()
  StoryHeader :: Eff v Header

data Bin = BAdd | BSub | BMul | BDiv | BAnd
  deriving Show
