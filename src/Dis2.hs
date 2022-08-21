-- | Speculative disassembly based on tracing invalidity
module Dis2 (dis) where

import Numbers (Addr,Value)
import Operation (Operation,RoutineHeader,opLabels)
import Story (Story(header,size),OOB_Mode(..))
import Fetch (runFetch)
import Decode (fetchOperation,fetchRoutineHeader)
import qualified Operation as Op
import Header (Header(..))
import Text.Printf (printf)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

dis :: Story -> IO ()
dis story = do
  let endOfStory :: Addr = fromIntegral (size story)
  let Header{initialPC=_,staticMem} = Story.header story
  let (start,stop) = (staticMem,endOfStory)

  let xs = disBetween story (start,stop)

  let hs = [ i | i@Info{thing=HeaderDefaults{}} <- xs ]
  let os = [ i | i@Info{thing=Operation{}} <- xs ]

  let
    step :: Addr -> [Addr]
    step a = do
      case disOpM story a of
        Nothing -> []
        Just (Op.Jump a,_) -> [a]
        Just (op,after) -> do
          if isStopping op then [] else do
            [after] ++ branchesOf op

  let
    routineEntryAddresses = Set.fromList [ end | Info{end} <- hs ]
  let
    reachFromHeaders :: Set Addr
    reachFromHeaders = bfs step (Set.toList routineEntryAddresses)
  let
    reach :: Addr -> Bool
    reach a = a `Set.member` reachFromHeaders

    isEntry :: Addr -> Bool
    isEntry a = a `Set.member` routineEntryAddresses

  let ys = [ x | x@Info{begin} <- os , reach begin ]

  let
    overlaps =
      [ end > next
      | (Info{end},Info{begin=next}) <- (zip ys (tail ys))
      ] ++ [False]

  let yos = [ i | i@Info{thing=Operation{}} <- ys ]

  printf "Found %d headers\n" (length hs)
  printf "Found %d operations; %d overlapped\n" (length yos) (length [ () | b <- overlaps, b ])

  flip mapM_ (zip ys overlaps) $ \(i@Info{begin},overlap) -> do
    let eMarker = if isEntry begin then "* " else "  "
    let oMarker = if overlap then " !" else ""
    putStrLn (eMarker ++ show i ++ oMarker)


type VM = Map Addr Bool

disBetween :: Story -> (Addr,Addr) -> [Info]
disBetween story (start,stop) = do
  let allA = [ start .. stop - 1 ]
  let vm = Map.fromList [ (a, validOp vm story a) | a <- allA ]
  [ i | a <- allA, i <- disInfo vm story a ]

disInfo :: VM -> Story -> Addr -> [Info]
disInfo vm story addr = do
  let begin = addr
  let opM = disOpM story addr
  let validO = validOp vm story addr
  let headM = disRoutineHeader story addr
  let validH = validRH vm story addr
  (case opM of
    Nothing -> []
    Just (op,end) | validO -> do
      [Info { begin, thing = Operation op, end }]
    _  -> []
    ) ++
    (case headM of
      Nothing -> []
      Just (Op.BadRoutineHeader{}, _) -> []
      Just (Op.RoutineHeader defs, end) | validH -> do
        [Info { begin, thing = HeaderDefaults defs, end }]
      _  -> [])


validRH :: VM -> Story -> Addr -> Bool
validRH vm story a = do
  case disRoutineHeader story a of
    Nothing -> False
    Just (_,a') -> validOp vm story a'

validOp :: VM -> Story -> Addr -> Bool
validOp vm story = recurse
  where
    recurse :: Addr -> Bool
    recurse a =
      case disOpM story a of
        Nothing -> False
        Just (Op.Jump a',_) -> callRecurse a'
        Just (op,next) -> do
          case directCall op of
            Just rha -> do
              case disRoutineHeader story rha of
                Nothing -> do
                  False
                Just (_,routineStart) -> do
                  callRecurse routineStart && callRecurse next
            Nothing -> do
              isStopping op ||
                all id [ callRecurse a'
                       | a' <- [ next ] ++ branchesOf op
                       ]
      where
        callRecurse :: Addr -> Bool
        callRecurse a' =
          if a' > a
          then maybe False id $ Map.lookup a' vm
          else True -- break recursive loops


directCall :: Operation -> Maybe Addr
directCall = \case
  Op.Call (Op.Floc a) _ _ -> Just a
  Op.CallN (Op.Floc a) _ -> Just a
  _ -> Nothing


disOpM :: Story -> Addr -> Maybe (Operation,Addr)
disOpM story a = do
  let (op,a') = runFetch OOB_Zero a story fetchOperation
  case isBadOp story op of
    True -> Nothing
    False -> Just (op, a')

disRoutineHeader :: Story -> Addr -> Maybe (RoutineHeader,Addr)
disRoutineHeader story a = do
  let (header,a') = runFetch OOB_Zero a story fetchRoutineHeader
  case header of
    Op.BadRoutineHeader -> Nothing
    Op.RoutineHeader{} -> Just (header, a')

data Info = Info
  { begin :: Addr
  , thing :: Thing
  , end :: Addr
  }

data Thing = Operation Operation | HeaderDefaults [Value]

instance Show Info where
  show Info{begin,thing,end} = do
    ljust (show begin ++ " " ++ show thing) 70 ++ show end

ljust :: String -> Int -> String
ljust s n = s ++ spaces (n - length s)

spaces :: Int -> String
spaces n = [ ' ' | _ <- [0..n-1] ]

instance Show Thing where
  show = \case
    HeaderDefaults xs -> "----------Header: " ++ show xs ++ "----------"
    Operation op -> show op


----------------------------------------------------------------------
-- copied from Disassemble

isBadOp :: Story -> Operation -> Bool
isBadOp story = \case
  Op.BadOperation{} -> True
  Op.Call Op.BadFunc{} _ _ -> True
  Op.Call (Op.Floc a) _ _ -> a >= size story
  Op.CallN Op.BadFunc{} _ -> True
  Op.CallN (Op.Floc a) _ -> a >= size story
  _ -> False

branchesOf :: Operation -> [Addr]
branchesOf = \case
  Op.Jump a -> [a]
  op -> [ a | Op.Branch _ (Op.Dloc a) <- opLabels op ]

isStopping :: Operation -> Bool
isStopping = \case
  Op.Jump{} -> True
  Op.Print_ret{} -> True
  Op.Quit{} -> True
  Op.Ret_popped -> True
  Op.Ret{} -> True
  Op.Rfalse{} -> True
  Op.Rtrue{} -> True
  _ -> False

----------------------------------------------------------------------
-- from AoC

bfs :: Ord a => (a -> [a]) -> [a] -> Set a
bfs step initial = loop Set.empty (Set.fromList initial)
  where
    loop acc frontier = do
      if Set.null frontier then acc else do
        let acc' = acc `Set.union` frontier
        let frontier' = Set.fromList [ a2 | a1 <- Set.toList frontier
                                          , a2 <- step a1, a2 `notElem` acc' ]
        loop acc' frontier'
