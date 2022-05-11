
module Fetch (Fetch(..),runFetch) where

import Control.Monad (ap,liftM)
import Numbers (Byte,Addr)
import Story (Story,readStoryByte)

instance Functor Fetch where fmap = liftM
instance Applicative Fetch where pure = return; (<*>) = ap
instance Monad Fetch where return = Ret; (>>=) = Bind

data Fetch a where
  Ret :: a -> Fetch a
  Bind :: Fetch a -> (a -> Fetch b) -> Fetch b
  NextByte :: Fetch Byte
  Here :: Fetch Addr
  GetByte :: Addr -> Fetch Byte
  WithPC :: Addr -> Fetch a -> Fetch a

runFetch :: Addr -> Story -> Fetch a -> (a, Addr)
runFetch pc0 story eff = loop pc0 eff $ \pc x -> (x,pc)
  where
    loop :: Addr -> Fetch a -> (Addr -> a -> r) -> r
    loop pc eff0 k = case eff0 of
      Ret a -> k pc a
      Bind e f -> loop pc e $ \pc a -> loop pc (f a) k
      NextByte -> k (pc+1) (readStoryByte story pc)
      Here -> k pc pc
      GetByte a -> k pc (readStoryByte story a)
      WithPC pc' e -> loop pc' e $ \_ a -> k pc a
