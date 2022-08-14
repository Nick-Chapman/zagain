
-- | Explore the static structure of a strory file
module Disassemble
  ( disassemble
  , dynamicDiscovery, routinesBetween
  , Routine(..), disRoutine, dumpRoutine
  , branchesOf, isStopping
  ) where

import Action (Action(..))
import Control.Monad (when)
import Data.Bits ((.&.),complement)
import Data.List ((\\),nub,sortBy,sort)
import Data.List.Extra (notNull)
import Data.Ord (comparing)
import Data.Set (Set)
import Decode (fetchOperation,fetchRoutineHeader)
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Byte,Addr)
import Operation (Operation,RoutineHeader,opLocals,opLabels)
import Story (Story(header,size),OOB_Mode(..))
import Text.Printf (printf)
import qualified Data.Set as Set
import qualified Interpreter (runEffect)
import qualified Operation as Op
import qualified Semantics (smallStep)

disassemble :: Story -> [String] -> IO ()
disassemble story walkthrough = do
  seeMemMap story
  discoverCode story walkthrough

seeMemMap :: Story -> IO ()
seeMemMap story = do
  let Header{ zv = _
            , abbrevTable, objectTable, globalVars
            , staticMem, dictionary, highMem, initialPC
            } = Story.header story
  let endOfStory :: Addr = fromIntegral (size story)
  putStrLn "Memory Map:"
  putStrLn "--[dynamic]---------------------------"
  pr ("Abbreviations",abbrevTable)
  pr ("Object",objectTable)
  pr ("Globals",globalVars)
  putStrLn "--[static]----------------------------"
  pr ("Static memory",staticMem)
  pr ("Dictionary",dictionary)
  pr ("High memory",highMem)
  pr ("Initial PC",initialPC)
  pr ("End of story file",endOfStory)
  putStrLn "--------------------------------------"
    where pr (tag,a) = printf "%s %s\n" (show a) tag


discoverCode :: Story -> [String] -> IO ()
discoverCode story walkthrough = do
  let endOfStory :: Addr = fromIntegral (size story)
  let Header{initialPC=_, staticMem} = Story.header story

  let ws = nub $ dynamicDiscovery story walkthrough
  let cs = routinesBetween story (staticMem,endOfStory)
  let lost = ws \\ cs

  printf "Found %d routines by walkthrough\n" (length ws)
  printf "Found %d routines by speculative disassembly\n" (length cs)
  printf "%d routines lost (in walkthrough but not disassembly)\n" (length lost)
  --print lost

  let foundR = [ (Found, disRoutine story a) | a <- cs ]
  let lostR = [ (Lost, disRoutine story a) | a <- lost ]

  let lostAndFound = sortBy (comparing rStart) (lostR ++ foundR)
        where rStart (_,Routine{start}) = start

  let prevs = 0 : [ finish | (_,Routine{finish}) <- lostAndFound ]
  sequence_ [ dumpRoutine0 prev lf r | (prev,(lf,r)) <- zip prevs lostAndFound ]


----------------------------------------------------------------------
-- dump

data LF = Lost | Found

dumpRoutine0 :: Addr -> LF -> Routine -> IO ()
dumpRoutine0 prev lf r = do
  let Routine{start} = r
  let gap :: Int = fromIntegral (start - prev)
  printf "--------------------------------------------------\n"
  printf "gap=%d%s\n" gap (if gap<0 then " **OVERLAP**" else "")
  let tag = case lf of Lost -> "**LOST**"; Found -> ""
  printf "%s" tag
  dumpRoutine r

dumpRoutine :: Routine -> IO ()
dumpRoutine r = do
  let Routine{start,header,body=xs} = r
  printf "%s %s\n" (show start) (show header)
  let Routine{illegal,unused} = r
  --printf "-- local defs: %s\n" (show defs)
  --printf "-- local refs locals: %s\n" (show refs)
  when (notNull illegal) $ printf "-- illegal: %s\n" (show illegal)
  when (notNull unused ) $ printf "-- unused: %s\n" (show unused)
  mapM_ pr xs
  where pr (a,op) = printf "%s %s\n" (show a) (show op)

----------------------------------------------------------------------
-- dynamic discovery from walkthrough (from comparison)

dynamicDiscovery :: Story -> [String] -> [Addr]
dynamicDiscovery story walkthrough = do
  let seed = 888
  let eff = Semantics.smallStep
  let screenWidth = 80
  let action = Interpreter.runEffect screenWidth seed story eff
  collectRoutineCalls walkthrough action

collectRoutineCalls :: [String] -> Action -> [Addr]
collectRoutineCalls = loop
  where
    loop :: [String] -> Action -> [Addr]
    loop coms = \case
      Tab a -> loop coms a
      UnTab a -> loop coms a
      Stop{} ->
        case coms of
          _:_ -> error "collectRoutineCalls: inputs left over"
          [] -> []
      TraceInstruction _ _ _ _ next -> loop coms next
      TraceRoutineCall addr next -> do addr : loop coms next -- collect
      Debug _ next -> loop coms next
      TextStyle _ next -> loop coms next
      Output _ next -> loop coms next
      Input _ _ f -> do
        case coms of
          com:coms -> loop coms (f com)
          [] -> []

----------------------------------------------------------------------
-- candidate...

routinesBetween :: Story -> (Addr,Addr) -> [Addr]
routinesBetween story (start,end) = loop start
  where
    loop :: Addr -> [Addr]
    loop a0 = do
      let a = (a0+1) .&. complement 1 -- align to 2 byte boundary
      if a > end then [] else do
        case tryDecodeRoutine story a of
          Nothing -> loop (a+1)
          Just a' -> a : loop a'

tryDecodeRoutine :: Story -> Addr -> Maybe Addr
tryDecodeRoutine story a = do
  case disRoutineM story a of
    Left{} -> Nothing
    Right Routine{finish,illegal,unused,defs=_} -> if
      | length illegal == 0
        && length unused <= 4 -- TODO: testing unused is a heuristic which might be wrong
        -> Just finish
      | otherwise
        -> Nothing

----------------------------------------------------------------------
-- disassembly...

data Routine = Routine
  { start :: Addr
  , header :: RoutineHeader
  , body :: [(Addr,Operation)]
  , finish :: Addr
  -- local variable usage
  , defs :: [Byte]
  , refs :: [Byte]
  , illegal :: [Byte]
  , unused :: [Byte]
  }

disRoutine :: Story -> Addr -> Routine
disRoutine s a =
  case disRoutineM s a of
    Right r -> r
    Left m -> error (show("disRoutine",a,m))


type Result a = Either String a

disRoutineM :: Story -> Addr -> Result Routine
disRoutineM story start = do
  let (header,a0) = runFetch OOB_Zero start story fetchRoutineHeader
  case header of
    Op.BadRoutineHeader{} -> Left (show header)
    Op.RoutineHeader xs -> do
      case (loop Set.empty [] a0) of
        Left m -> Left m
        Right (body,finish) -> do
          let defs = [ b | b <- [1::Byte .. fromIntegral (length xs) ] ]
          let refs = sort $ nub [ b | (_,op) <- body, b <- opLocals op ]
          let illegal = refs \\ defs
          let unused = defs \\ refs
          Right $ Routine { start, header, body, finish, defs, refs, illegal, unused }
      where
        loop :: Set Addr -> [(Addr,Operation)] -> Addr -> Result ([(Addr,Operation)], Addr)
        loop bps acc a = do
          let (i,a') = runFetch OOB_Zero a story fetchOperation
          case i of
            Op.BadOperation{} -> Left (show (a,i))
            Op.Call Op.BadFunc{} _ _ -> Left (show (a,i))
            Op.CallN Op.BadFunc{} _ -> Left (show (a,i))
            _ -> do
              let bps' :: Set Addr = bps `Set.union` Set.fromList (branchesOf i)
              let continue = not (isStopping i) || a' `Set.member` bps
              if continue then loop bps' ((a,i):acc) a' else
                Right (reverse((a,i):acc),a')


branchesOf :: Operation -> [Addr]
branchesOf = \case
  Op.Jump a -> [a]
  op -> [ a | Op.Branch _ (Op.Dloc a) <- opLabels op ]

isStopping :: Operation -> Bool
isStopping = \case
  Op.Jump{} -> True
  Op.Print_ret{} -> True
  Op.Ret_popped -> True
  Op.Ret{} -> True
  Op.Rfalse{} -> True
  Op.Rtrue{} -> True
  _ -> False
