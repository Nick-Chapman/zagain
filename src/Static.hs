
-- | Explore the static structure of a strory file
module Static (explore) where

--import Control.Monad (when)
--import Data.List (sortBy)
--import Data.List.Extra (nubSortBy)
--import Data.Ord (comparing)
import Data.Set (Set)
import Decode (fetchOperation,fetchRoutineHeader)
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Addr)
import Operation (Operation)
import Story (Story(header,size),OOB_Mode(..))
import Text.Printf (printf)
import qualified Data.Set as Set
import qualified Operation as Op

explore :: Story -> IO ()
explore story = do
  seeMemMap story
  discoverCode story

seeMemMap :: Story -> IO ()
seeMemMap story = do
  let Header{ zv = _
            , abbrevTable, objectTable, globalVars
            , staticMem, dictionary, highMem, initialPC
            } = Story.header story
  let endOfStory :: Addr = fromIntegral (size story)
  putStrLn "--[dynamic]---------------------------"
  pr ("abbrevTable",abbrevTable)
  pr ("objectTable",objectTable)
  pr ("globalVars",globalVars)
  putStrLn "--[static]----------------------------"
  pr ("staticMem",staticMem)
  pr ("dictionary",dictionary)
  pr ("highMem",highMem)
  pr ("initialPC",initialPC)
  pr ("endOfStory",endOfStory)
  putStrLn "--------------------------------------"
    where pr (tag,a) = printf "%s %s\n" (show a) tag


discoverCode :: Story -> IO ()
discoverCode story = do
  let endOfStory :: Addr = fromIntegral (size story)
  let Header{initialPC=_, staticMem} = Story.header story
  -- address which might contain routines...
  let xs0 = [staticMem .. endOfStory]
  print("0",length xs0)
  let xs1 = [ x | x <- xs0, x `mod` 2 == 0 ]
  print("1",length xs1)
  let xs2 = [ x | x <- xs1, canDecodeRoutine story x ]
  print("2",length xs2)
  let xs3 = routinesBetween story (staticMem,endOfStory)
  print("3",length xs3)
  pure ()

routinesBetween :: Story -> (Addr,Addr) -> [Addr]
routinesBetween story (start,end) = loop start
  where
    loop :: Addr -> [Addr]
    loop a =
      if a > end then [] else do
        case tryDecodeRoutine story a of
          Nothing -> loop (a+1)
          Just a' -> a : loop a'

canDecodeRoutine :: Story -> Addr -> Bool
canDecodeRoutine story a = do
  case tryDecodeRoutine story a of
    Nothing -> False
    Just{} -> True

tryDecodeRoutine :: Story -> Addr -> Maybe Addr
tryDecodeRoutine story a = do
  case tryDecodeRoutineHeader story a of
    Nothing -> Nothing
    Just a0 -> loop Set.empty a0
      where
        loop :: Set Addr -> Addr -> Maybe Addr
        loop bps a = do
          case tryDecodeOp story a of
            Nothing -> Nothing
            Just (op,a') -> do
              let bps' = bps `Set.union` Set.fromList (branchesOf op)
              let continue = not (isStopping op) || a' `Set.member` bps
              if continue then loop bps' a' else Just a'

tryDecodeRoutineHeader :: Story -> Addr -> Maybe Addr
tryDecodeRoutineHeader story a = do
  let (header,a') = runFetch OOB_Zero a story fetchRoutineHeader
  case header of
    Op.BadRoutineHeader -> Nothing
    Op.RoutineHeader{} -> Just a'

tryDecodeOp :: Story -> Addr -> Maybe (Operation,Addr)
tryDecodeOp story a = do
  let (op,a') = runFetch OOB_Zero a story fetchOperation
  case op of
    Op.BadOperation{} -> Nothing
    Op.Call Op.BadFunc{} _ _ -> Nothing
    op -> Just (op,a')

branchesOf :: Operation -> [Addr]
branchesOf = \case
  Op.Dec_chk _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Inc_chk _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Je _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Jg _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Jl _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Jump a -> [a]
  Op.Jz _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Test _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  Op.Test_attr _ _ (Op.Branch _ (Op.Dloc a)) -> [a]
  _ -> []

isStopping :: Operation -> Bool
isStopping = \case
  Op.Jump{} -> True
  Op.Print_ret{} -> True
  Op.Ret_popped -> True
  Op.Ret{} -> True
  Op.Rfalse{} -> True
  Op.Rtrue{} -> True
  _ -> False
