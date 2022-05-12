
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
  --GetByte :: Addr -> Fetch Byte
  WithPC :: Addr -> Fetch a -> Fetch a

runFetch :: Addr -> Story -> Fetch a -> (a, Addr, Int)
runFetch pc0 story eff = loop s0 eff $ \State{pc,readCount} x -> (x,pc,readCount)
  where
    s0 = State { pc = pc0, readCount = 0 }
    loop :: State -> Fetch a -> (State -> a -> r) -> r
    loop s@State{pc=here,readCount} eff0 k = case eff0 of
      Ret a -> k s a
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      NextByte ->
        k s { pc = here+1, readCount = readCount + 1 } (readStoryByte story here)
      Here -> k s here
      --GetByte a -> k s (readStoryByte story a)
      WithPC there e -> loop s { pc = there } e $ \s a -> k s {pc = here} a


data State = State { pc :: Addr, readCount :: Int }
