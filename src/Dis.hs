
module Dis (run1,run) where

import Prelude hiding (Word)

import Addr (Addr)
import Data.Word (Word16)
import Decode (fetchInstruction)
import Fetch (Fetch(..))
import Story (Story,loadStory,readStoryByte)
import Text.Printf (printf)
import qualified Instruction (pretty)

type Word = Word16

run1 :: IO () -- for current "make" regression
run1 = do
  let filename = "story/zork1.88-840726.z3"
  story <- loadStory filename
  let messages = disStory story
  mapM_ putStrLn messages

run :: IO ()
run = do
  let filename = "story/zork1.88-840726.z3"
  story <- loadStory filename
  let messages = disStory story
  mapM_ putStrLn messages

disStory :: Story -> [String]
disStory story = do
  let startA :: Addr = fromIntegral (readStoryWord story 0x6)
  runFetch startA story disRoutine

readStoryWord :: Story -> Addr -> Word
readStoryWord story a = do
  let hi = readStoryByte story a
  let lo = readStoryByte story (a+1)
  256 * fromIntegral hi + fromIntegral lo

disRoutine :: Fetch ()
disRoutine = sequence_ (repeat disLocatedI)

disLocatedI :: Fetch ()
disLocatedI = do
  a <- Here
  i <- fetchInstruction
  Trace (printf "[%s] %s" (show a) (Instruction.pretty i))

runFetch :: Addr -> Story -> Fetch () -> [String]
runFetch pc0 story eff = loop pc0 eff $ \_ () -> []
  where
    loop :: Addr -> Fetch a -> (Addr -> a -> [String]) -> [String]
    loop pc eff0 k = case eff0 of
      Ret a -> k pc a
      Bind e f -> loop pc e $ \pc a -> loop pc (f a) k
      Trace mes -> mes : k pc ()
      NextByte -> k (pc+1) (readStoryByte story pc)
      Here -> k pc pc
