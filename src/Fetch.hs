
module Fetch (Fetch(..)) where

import Control.Monad (ap,liftM)
import Numbers (Byte,Addr)

instance Functor Fetch where fmap = liftM
instance Applicative Fetch where pure = return; (<*>) = ap
instance Monad Fetch where return = Ret; (>>=) = Bind

data Fetch a where -- TODO: move into Decode?
  Ret :: a -> Fetch a
  Bind :: Fetch a -> (a -> Fetch b) -> Fetch b
  NextByte :: Fetch Byte
  Here :: Fetch Addr
  Err :: String -> Fetch a
  GetByte :: Addr -> Fetch Byte
  WithPC :: Addr -> Fetch a -> Fetch a
