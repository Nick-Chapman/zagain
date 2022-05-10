
-- The effect of evaluation on a z-machine
module Eff (Eff(..),Bin(..)) where

import Control.Monad (ap,liftM)
import Instruction (Instruction,Target,RoutineHeader)
import Numbers (Byte,Addr,Value)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b

  Debug :: Show a => a -> Eff ()

  GamePrint :: String -> Eff ()
  ReadInputFromUser :: Eff String

  FetchI :: Eff Instruction
  FetchHeader :: Eff RoutineHeader

  PushFrame :: Addr -> Target -> Eff ()
  PopFrame :: Eff Target
  SetPC :: Addr -> Eff ()

  GetLocal :: Byte -> Eff Value
  SetLocal :: Byte -> Value -> Eff ()

  EqualAny :: [Value] -> Eff Bool
  IsZero :: Value -> Eff Bool
  BinOp :: Bin -> Value -> Value -> Eff Value

  GetByte :: Addr -> Eff Byte
  SetByte :: Addr -> Byte -> Eff ()

  PushStack :: Value -> Eff ()
  PopStack :: Eff Value

data Bin = BAdd | BSub | BAnd
  deriving Show
