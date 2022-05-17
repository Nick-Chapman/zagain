
-- | Disassemble statically reachable z-code routines from a stroy file.
module Dis (disassemble) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Decode (fetchOperation,fetchRoutineHeader)
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Addr)
import Operation (Operation,RoutineHeader)
import Story (Story(header))
import Text.Printf (printf)
import qualified Data.Set as Set
import qualified Operation as Op

disassemble :: Story -> IO ()
disassemble story = do
  let Header{initialPC} = Story.header story
  let a0 :: Addr = initialPC - 1
  -- extra places not picked up by reachability...
  let extra = [65784] -- [20076,20386,20688,21700]
  let startingPoints = [a0] ++ extra
  let rs = sortBy (comparing start) $ collectRoutines startingPoints story
  printf "Found %d reachable routines:\n" (length rs)
  mapM_ dumpRoutine rs

dumpRoutine :: Routine -> IO ()
dumpRoutine Routine{start,header,body=xs,finish=_} = do
  -- TODO: show gap between routines
  printf "--------------------------------------------------\n"
  printf "[%s] %s\n" (show start) (show header)
  mapM_ pr xs
  --printf "[%s]\n" (show finish)
  where
    pr (a,i) = printf "[%s] %s\n" (show a) (Op.pretty i)

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
collectRoutine start story = do
  let (header,a0,_) = runFetch start story fetchRoutineHeader
  let (body,finish) = loop Set.empty [] a0
  Routine { start, header, body, finish }
  where
    loop :: Set Addr -> [(Addr,Operation)] -> Addr -> ([(Addr,Operation)], Addr)
    loop bps acc a = do
      let (i,a',_) = runFetch a story fetchOperation
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
