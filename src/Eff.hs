
-- | The computation effect of z-machine execution.
module Eff (Eff(..),Bin(..)) where

import Control.Monad (ap,liftM)
import Dictionary (Dict)
import Numbers (Byte,Addr,Value)
import Operation (Operation,Target,RoutineHeader)

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  Debug :: Show a => a -> Eff ()
  GamePrint :: String -> Eff ()
  ReadInputFromUser :: Eff String
  GetText :: Addr -> Eff String
  FetchI :: Eff Operation
  FetchHeader :: Eff RoutineHeader
  FetchDict :: Eff Dict
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
  Random :: Int -> Eff Int

data Bin = BAdd | BSub | BMul | BDiv | BAnd
  deriving Show
