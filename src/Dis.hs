
module Dis (disZork) where

import Prelude hiding (Word)

import Addr (Addr)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Set as Set
import Data.Word (Word16)
import Decode (fetchInstruction,fetchRoutineHeader)
import Fetch (Fetch(..))
import Instruction (Instruction,pretty,RoutineHeader)
import Story (Story,loadStory,readStoryByte)
import Text.Printf (printf)
import qualified Instruction as I

type Word = Word16

disZork :: IO ()
disZork = do
  let filename = "story/zork1.88-840726.z3"
  story <- loadStory filename
  let a0 :: Addr = fromIntegral (readStoryWord story 0x6) - 1 -- back 1 for the header
  -- extra places not picked up by reachability...
  let extra = [] -- [20076,20386,20688,21700]
  let startingPoints = [a0] ++ extra
  let rs = sortBy (comparing start) $ collectRoutines startingPoints story
  printf "Found %d reachable routines:\n" (length rs)
  mapM_ dumpRoutine rs

readStoryWord :: Story -> Addr -> Word
readStoryWord story a = do
  let hi = readStoryByte story a
  let lo = readStoryByte story (a+1)
  256 * fromIntegral hi + fromIntegral lo

dumpRoutine :: Routine -> IO ()
dumpRoutine Routine{start,header,body=xs,finish=_} = do
  printf "--------------------------------------------------\n"
  printf "[%s] %s\n" (show start) (show header)
  mapM_ pr xs
  --printf "[%s]\n" (show finish)
  where
    pr (a,i) = printf "[%s] %s\n" (show a) (Instruction.pretty i)

collectRoutines :: [Addr] -> Story -> [Routine]
collectRoutines as story = loop Set.empty [] as
  where
    loop :: Set Addr -> [Routine] -> [Addr] -> [Routine]
    loop done acc = \case
      [] -> acc
      a:todo -> do
        if a `Set.member` done then loop done acc todo else do
          let r = collectRoutine a story
          let done' = Set.insert a done
          let todo' = callAddresses r ++ todo
          loop done' (r:acc) todo'

collectRoutine :: Addr -> Story -> Routine
collectRoutine start story =
  case runFetch start story fetchRoutineHeader of
    Left{} -> undefined
    Right (header,a0) -> do
      let (body,finish) = loop Set.empty [] a0
      Routine { start, header, body, finish }
  where
    loop :: Set Addr -> [(Addr,Instruction)] -> Addr -> ([(Addr,Instruction)], Addr)
    loop bps acc a = do
      case runFetch a story fetchInstruction of
        Left s -> (reverse ((a,I.Bad s):acc), a)
        Right (i,a') -> do
          let bps' :: Set Addr = bps `Set.union` Set.fromList (branchesOfI i)
          let continue = not (isStoppingI i) || a' `Set.member` bps
          if continue then loop bps' ((a,i):acc) a' else (reverse((a,i):acc),a')

callAddresses :: Routine -> [Addr]
callAddresses Routine{body=xs} = do
  [ a | (_,i) <- xs, a <- callsOfI i ]

callsOfI :: Instruction -> [Addr]
callsOfI = \case
  I.Call (I.Floc a) _ _ -> [a]
  _ -> []

branchesOfI :: Instruction -> [Addr]
branchesOfI = \case
  I.Dec_check _ _ (I.Branch _ (I.Dloc a)) -> [a]
  I.Inc_check _ _ (I.Branch _ (I.Dloc a)) -> [a]
  I.Je _ (I.Branch _ (I.Dloc a)) -> [a]
  I.Jg _ _ (I.Branch _ (I.Dloc a)) -> [a]
  I.Jl _ _ (I.Branch _ (I.Dloc a)) -> [a]
  I.Jump a -> [a]
  I.Jz _ (I.Branch _ (I.Dloc a)) -> [a]
  I.Test _ _ (I.Branch _ (I.Dloc a)) -> [a]
  I.Test_attr _ _ (I.Branch _ (I.Dloc a)) -> [a]
  _ -> []

isStoppingI :: Instruction -> Bool
isStoppingI = \case
  I.Jump{} -> True
  I.Print_ret{} -> True
  I.Ret_popped -> True
  I.Return{} -> True
  I.Rfalse{} -> True
  I.Rtrue{} -> True
  _ -> False


data Routine = Routine
  { start :: Addr
  , header :: RoutineHeader
  , body :: [(Addr,Instruction)]
  , finish :: Addr
  }

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
