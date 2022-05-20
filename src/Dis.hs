
-- | Disassemble statically reachable z-code routines from a stroy file.
module Dis (disassemble) where

import Control.Monad (when)
import Data.List (sortBy)
import Data.List.Extra (nubSortBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Decode (fetchOperation,fetchRoutineHeader)
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Addr)
import Operation (Operation,RoutineHeader)
import Story (Story(header,size))
import Text.Printf (printf)
import qualified Data.Set as Set
import qualified Operation as Op

disassemble :: Story -> IO ()
disassemble story = do
  let Header{initialPC,staticMem} = Story.header story
  let a0 :: Addr = initialPC - 1
  let startingPoints = [a0]
  let rs1 = sortBy (comparing start) $ collectRoutines startingPoints story
  printf "Found %d reachable routines:\n" (length rs1)

  -- TODO: make it work even when we go to the very end of the story file
  let _end :: Addr = fromIntegral $ size story
  let end = 84000
  let rs2 = collectCandidateRoutinesInRange (staticMem, end) story
  printf "Found %d candidate routines:\n" (length rs2)

  let rs = nubSortBy (comparing start) $ (rs1 ++ rs2)
  printf "Found %d combined routines:\n" (length rs)
  -- TODO: show gaps (which might be negative)
  mapM_ dumpRoutine rs


dumpRoutine :: Routine -> IO ()
dumpRoutine Routine{start,header,body=xs,finish=_} = do
  -- TODO: show gap between routines
  printf "--------------------------------------------------\n"
  printf "%s %s\n" (show start) (show header)
  mapM_ pr xs
  --printf "[%s]\n" (show finish)
  where
    pr (a,op) = printf "%s %s\n" (show a) (show op)


--[candidate routines]------------------------------------------------

collectCandidateRoutinesInRange :: (Addr,Addr) -> Story -> [Routine]
collectCandidateRoutinesInRange (a0,a1) story = do
  loop a0
  where
    loop a = do
      if a >= a1 then [] else do
        let r@Routine{finish} = collectRoutine False a story
        if isGoodR r then r : loop finish else loop (a+1)

isGoodR :: Routine -> Bool
isGoodR = \case
  Routine{header=Op.BadRoutineHeader} -> False
  Routine{body=xs} ->
    all isGoodOp [ op | (_,op) <- xs ]

isGoodOp :: Operation -> Bool
isGoodOp = \case
  Op.BadOperation{} -> False
  Op.Call Op.BadFunc{} _ _ -> False
  _ -> True


--[reachable routines]------------------------------------------------

collectRoutines :: [Addr] -> Story -> [Routine]
collectRoutines as story = loop Set.empty [] as
  where
    loop :: Set Addr -> [Routine] -> [Addr] -> [Routine]
    loop done acc = \case
      [] -> acc
      a:todo -> do
        if a `Set.member` done then loop done acc todo else do
          let r = collectRoutine True a story
          let done' = Set.insert a done
          let todo' = callAddresses r ++ todo
          loop done' (r:acc) todo'

collectRoutine :: Bool -> Addr -> Story -> Routine
collectRoutine strict start story = do
  let (header,a0,_) = runFetch start story fetchRoutineHeader
  let (body,finish) = loop Set.empty [] a0
  Routine { start, header, body, finish }
  where
    loop :: Set Addr -> [(Addr,Operation)] -> Addr -> ([(Addr,Operation)], Addr)
    loop bps acc a = do
      let (i,a',_) = runFetch a story fetchOperation
      when strict $
        case i of
          Op.BadOperation m -> error ("collect: " ++ m)
          _ -> pure ()
      let bps' :: Set Addr = bps `Set.union` Set.fromList (branchesOfI i)
      let continue = not (isStoppingI i) || a' `Set.member` bps
      if continue then loop bps' ((a,i):acc) a' else (reverse((a,i):acc),a')

callAddresses :: Routine -> [Addr]
callAddresses Routine{body=xs} = do
  [ a | (_,i) <- xs, a <- callsOfI i ]

callsOfI :: Operation -> [Addr]
callsOfI = \case
  Op.Call (Op.Floc a) _ _ -> [a]
  _ -> []

branchesOfI :: Operation -> [Addr]
branchesOfI = \case
  Op.Dec_check _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Inc_check _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Je _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Jg _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Jl _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Jump a -> [a]
  Op.Jz _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Test _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Test_attr _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  _ -> []

isStoppingI :: Operation -> Bool
isStoppingI = \case
  Op.Jump{} -> True
  Op.Print_ret{} -> True
  Op.Ret_popped -> True
  Op.Return{} -> True
  Op.Rfalse{} -> True
  Op.Rtrue{} -> True
  _ -> False


data Routine = Routine
  { start :: Addr
  , header :: RoutineHeader
  , body :: [(Addr,Operation)]
  , finish :: Addr
  }

