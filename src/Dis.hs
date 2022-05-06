
module Dis (runAll,runReach) where

import Prelude hiding (Word)

import Addr (Addr)
import Data.Word (Word16)
import Decode (fetchInstruction,fetchRoutineHeader)
import Fetch (Fetch(..))
import Instruction (Instruction,pretty,RoutineHeader)
import Story (Story,loadStory,readStoryByte)
import Text.Printf (printf)
import qualified Instruction as I

type Word = Word16

readStoryWord :: Story -> Addr -> Word
readStoryWord story a = do
  let hi = readStoryByte story a
  let lo = readStoryByte story (a+1)
  256 * fromIntegral hi + fromIntegral lo

----------------------------------------------------------------------
runAll :: IO ()
runAll = do
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

----------------------------------------------------------------------
runReach :: IO ()
runReach = do
  let filename = "story/zork1.88-840726.z3"
  story <- loadStory filename
  let a0 :: Addr = fromIntegral (readStoryWord story 0x6) - 1 -- back 1 for the header
  let r0 = collectRoutine a0 story
  dumpRoutine r0
  let a1 :: Addr = 20386 -- hack
  let r1 = collectRoutine a1 story
  dumpRoutine r1
  case callAddresses r0 of
    [] -> print "*no calls"
    a2:_ -> do
      let r2 = collectRoutine a2 story
      let _ = dumpRoutine r2 --next
      pure ()

collectRoutine :: Addr -> Story -> Routine
collectRoutine a story =
  case runFetch a story fetchRoutineHeader of
    Left{} -> undefined
    Right (header,a') -> do
      let body = loop a'
      Routine { start=a, header, body }
  where
    loop :: Addr -> [(Addr,Instruction)]
    loop a = do
      case runFetch a story fetchInstruction of
        Left s -> [(a,I.Bad s)]
        Right (i,a') -> (a,i) : loop a'

callAddresses :: Routine -> [Addr]
callAddresses Routine{body=xs} = do
  [ a | (_,i) <- xs, a <- callsOfI i ]

callsOfI :: Instruction -> [Addr]
callsOfI = \case
  I.Call (I.Floc a) _ _ -> [a]
  _ -> []


----------------------------------------------------------------------
data Routine = Routine
  { start :: Addr
  , header :: RoutineHeader
  , body :: [(Addr,Instruction)]
  }

dumpRoutine :: Routine -> IO ()
dumpRoutine Routine{start,header,body=xs} = do
  printf "--------------------------------------------------\n"
  printf "[%s] %s\n" (show start) (show header)
  mapM_ pr xs
  where
    pr (a,i) = printf "[%s] %s\n" (show a) (Instruction.pretty i)

----------------------------------------------------------------------
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
      GetByte a -> k pc (readStoryByte story a)
      WithPC pc' e -> loop pc' e $ \_ a -> k pc a
