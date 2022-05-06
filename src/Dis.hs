
module Dis (run1,run) where

import Prelude hiding (Word)

import Addr (Addr)
import Data.Word (Word16)
import Decode (fetchInstruction)
import Fetch (Fetch(..))
import Instruction (Instruction)
import Story (Story,loadStory,readStoryByte)
import Text.Printf (printf)
import qualified Instruction (pretty)
import qualified Instruction as I

type Word = Word16

run1 :: IO () -- for current "make" regression
run1 = do
  let filename = "story/zork1.88-840726.z3"
  story <- loadStory filename
  let startA :: Addr = fromIntegral (readStoryWord story 0x6)
  disStory_stopOnError startA story

readStoryWord :: Story -> Addr -> Word
readStoryWord story a = do
  let hi = readStoryByte story a
  let lo = readStoryByte story (a+1)
  256 * fromIntegral hi + fromIntegral lo

disStory_stopOnError :: Addr -> Story -> IO ()
disStory_stopOnError a story = loop a
  where
    loop :: Addr -> IO ()
    loop a = do
      case runFetch a story fetchInstruction of
        Left{} -> pure ()
        Right (i,a') -> do
          printf "[%s] %s\n" (show a) (Instruction.pretty i)
          loop a'


run :: IO ()
run = do
  let filename = "story/zork1.88-840726.z3"
  story <- loadStory filename
  let startA :: Addr = 0
  disStory startA story

disStory :: Addr -> Story -> IO ()
disStory startA story = do
  let js = [ (a,i) | (a,i) <- collectInstructions startA story, notBad i ]
  mapM_ pr js
    where
      notBad = \case I.Bad{} -> False; _ -> True
      pr (a,i) = printf "[%s] %s\n" (show a) (Instruction.pretty i)

collectInstructions :: Addr -> Story -> [(Addr,Instruction)]
collectInstructions a story = loop a
  where
    loop :: Addr -> [(Addr,Instruction)]
    loop a = do
      case runFetch a story fetchInstruction of
        Left s -> (a,I.Bad s) : loop (a+1)
        Right (i,a') -> (a,i) : loop a'


runFetch :: Addr -> Story -> Fetch a -> Either String (a, Addr)
runFetch pc0 story eff = loop pc0 eff $ \pc x -> Right (x,pc)
  where
    loop :: Addr -> Fetch a -> (Addr -> a -> Either String r) -> Either String r
    loop pc eff0 k = case eff0 of
      Ret a -> k pc a
      Bind e f -> loop pc e $ \pc a -> loop pc (f a) k
      NextByte -> k (pc+1) (readStoryByte story pc)
      Here -> k pc pc
      Err s -> Left s
