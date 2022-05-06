
module Fetch (Fetch(..)) where

import Addr(Addr)
import Control.Monad (ap,liftM)
import Data.Word (Word8)

type Byte = Word8

instance Functor Fetch where fmap = liftM
instance Applicative Fetch where pure = return; (<*>) = ap
instance Monad Fetch where return = Ret; (>>=) = Bind

data Fetch a where
  Ret :: a -> Fetch a
  Bind :: Fetch a -> (a -> Fetch b) -> Fetch b
  NextByte :: Fetch Byte
  Here :: Fetch Addr
  Err :: String -> Fetch a
