
-- | The effect of decoding elements of a z-machine story-file.
module Fetch (Fetch(..),runFetch) where

import Control.Monad (ap,liftM)
import Header (Header)
import Numbers (Byte,Addr)
import Story (Story(header),readStoryByte,OOB_Mode(..))

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
  StoryHeader :: Fetch Header

runFetch :: OOB_Mode -> Addr -> Story -> Fetch a -> (a, Addr)
runFetch mode pc0 story eff = loop s0 eff $ \State{pc} x -> (x,pc)
  where
    s0 = State { pc = pc0 }
    loop :: State -> Fetch a -> (State -> a -> r) -> r
    loop s@State{pc=here} eff0 k = case eff0 of
      Ret a -> k s a
      Bind e f -> loop s e $ \s a -> loop s (f a) k
      NextByte -> k s { pc = here+1 } (readStoryByte mode story here)
      Here -> k s here
      GetByte a -> k s (readStoryByte mode story a)
      WithPC there e -> loop s { pc = there } e $ \s a -> k s {pc = here} a
      StoryHeader -> k s (Story.header story)

data State = State { pc :: Addr }
