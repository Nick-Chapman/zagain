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

  let xs = disBetween_dropIn story (start,stop)

  let hs = [ i | i@Info{thing=Thing_HeaderDefaults{}} <- xs ]
  let os = [ i | i@Info{thing=Thing_Operation{}} <- xs ]

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

  let yos = [ i | i@Info{thing=Thing_Operation{}} <- ys ]

  printf "Found %d headers\n" (length hs)
  printf "Found %d operations; %d overlapped\n" (length yos) (length [ () | b <- overlaps, b ])

  flip mapM_ (zip ys overlaps) $ \(i@Info{begin},overlap) -> do
    let eMarker = if isEntry begin then "* " else "  "
    let oMarker = if overlap then " !" else ""
    putStrLn (eMarker ++ show i ++ oMarker)


staticRangeOfStory :: Story -> (Addr,Addr)
staticRangeOfStory story = do
  let endOfStory :: Addr = fromIntegral (size story)
  let Header{initialPC=_,staticMem} = Story.header story
  (staticMem,endOfStory)

----------------------------------------------------------------------
-- info/thing

data Info = Info
  { begin :: Addr
  , thing :: Thing
  , end :: Addr
  }

data Thing = Thing_Operation Operation | Thing_HeaderDefaults [Value]

instance Show Info where
  show Info{begin,thing,end} = do
    ljust (show begin ++ " " ++ show thing) 70 ++ show end

ljust :: String -> Int -> String
ljust s n = s ++ spaces (n - length s)

spaces :: Int -> String
spaces n = [ ' ' | _ <- [0..n-1] ]

instance Show Thing where
  show = \case
    Thing_HeaderDefaults xs -> do
      "----------Header: " ++ show xs ++ "----------"
    Thing_Operation op -> do
      show op

----------------------------------------------------------------------

disBetween_dropIn :: Story -> (Addr,Addr) -> [Info]
disBetween_dropIn story (start,stop) = do
  let x = makeAnalysis story (start,stop)
  infosFromAnalysis x

infosFromAnalysis :: Analysis -> [Info]
infosFromAnalysis Analysis{headers,ops} =
  [ info
  | (begin,(Op.RoutineHeader defs, end)) <- Map.toList headers
  , let info = Info { begin, thing = Thing_HeaderDefaults defs, end }
  ] ++
  [ info
  | (begin,(op, end)) <- Map.toList ops
  , let info = Info { begin, thing = Thing_Operation op, end }
  ]

data Analysis = Analysis
  { headers :: Map Addr (RoutineHeader,Addr)
  , ops :: Map Addr (Operation,Addr)
  }

makeAnalysis :: Story -> (Addr,Addr) -> Analysis
makeAnalysis story (start,stop) = do
  let allA = [ start .. stop - 1 ]
  let
    vf0 = \_ -> True
    vf1 = validOpX story vf0
    vf = vf1
  let
    f_rh :: Addr -> Maybe (RoutineHeader,Addr)
    f_rh a =
      case disRoutineHeader story a of
        Nothing -> Nothing
        Just res@(_,a') ->
          if vf a'
          then Just res
          else Nothing

  let
    f_op :: Addr -> Maybe (Operation,Addr)
    f_op a =
      if vf a then disOpM story a else Nothing

  let headers = Map.fromList [ (a,res) | a <- allA, Just res <- [f_rh a] ]
  let ops = Map.fromList [ (a,res) | a <- allA, Just res <- [f_op a] ]
  Analysis { headers, ops }

----------------------------------------------------------------------

validOpX :: Story -> (Addr -> Bool) -> Addr -> Bool
validOpX story vfLast = do
  let (start,stop) = staticRangeOfStory story
  let allA = [ start .. stop - 1 ]
  let
    vm :: Map Addr Bool
    vm = Map.fromList [ (a, validOp_unfixed vf story a) | a <- allA ]
    vf :: Addr -> Bool
    vf addr = do
      if addr < start || addr >= stop then False else
        maybe undefined id $ Map.lookup addr vm
  vf
  where
    validOp_unfixed :: (Addr -> Bool) -> Story -> Addr -> Bool
    validOp_unfixed vf story = recurse
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
              then vf a'
              else vfLast a'


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
