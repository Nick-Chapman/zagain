-- | Speculative disassembly based on tracing invalidity
module Dis2 (disassemble) where

import Disassemble(seeMemMap)
import Numbers (Byte,Addr,Value)
import Operation (Operation,RoutineHeader,opLabels,Target(..),opTargets)
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


disassemble :: Story -> IO ()
disassemble story = do
  seeMemMap story
  dis story

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
    overlaps = [False] ++ -- overlap with next
      [ end > next
      | (Info{end},Info{begin=next}) <- (zip ys (tail ys))
      ] -- ++ [False] -- overlap with next

  let yos = [ i | i@Info{thing=Thing_Operation{}} <- ys ]

  printf "Found %d headers\n" (length hs)
  printf "Found %d operations; %d overlapped\n" (length yos) (length [ () | b <- overlaps, b ])

  flip mapM_ (zip ys overlaps) $ \(i@Info{begin},overlap) -> do
    let _eMarker = if isEntry begin then "* " else "  "
    let _oMarker = if overlap then "! " else "  "
    --putStrLn (_eMarker ++ show i ++ _oMarker)
    putStrLn (_eMarker ++ _oMarker ++ show i)
    --putStrLn (show i)


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
  show Info{begin,thing,end=_end} = do
    -- _ljust (show begin ++ " " ++ show thing) 70 ++ show _end
    show begin ++ " " ++ show thing

_ljust :: String -> Int -> String
_ljust s n = s ++ spaces (n - length s)

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
    vf0 = \_ -> Valid 0
    vf1 = validOpX story vf0
    vf2 = validOpX story vf1
    --vf3 = validOpX story vf2
    vf = vf2
  let
    f_rh :: Addr -> Maybe (RoutineHeader,Addr)
    f_rh a =
      case disRoutineHeader story a of
        Nothing -> Nothing
        Just res@(_,a') ->
          case vf a' of
            Valid{} -> Just res -- TODO: check enough formals
            Invalid -> Nothing

  let
    f_op :: Addr -> Maybe (Operation,Addr)
    f_op a =
      case vf a of
        Valid{} -> disOpM story a
        Invalid -> Nothing

  let headers = Map.fromList [ (a,res) | a <- allA, Just res <- [f_rh a] ]
  let ops = Map.fromList [ (a,res) | a <- allA, Just res <- [f_op a] ]
  Analysis { headers, ops }

----------------------------------------------------------------------

data Validity = Invalid | Valid Byte

andV :: Validity -> Validity -> Validity
andV = \case
  Invalid -> \_ -> Invalid
  Valid n -> \case
    Invalid -> Invalid
    Valid m -> Valid (max n m)

allV :: [Validity] -> Validity
allV = foldl andV (Valid 0)

{-orV :: Validity -> Validity -> Validity
orV = \case
  Valid -> \_ -> Valid
  Invalid -> \x -> x

anyV :: [Validity] -> Validity
anyV = foldl orV Invalid-}

withFormalsCount :: Int -> Validity -> Validity
withFormalsCount n = \case -- n is number of provided formals
  Invalid -> Invalid
  Valid m -> -- m is maximum local var referenced or assigned
    if fromIntegral n >= m then Valid 0 else Invalid


validOpX :: Story -> (Addr -> Validity) -> Addr -> Validity
validOpX story vfLast = do
  let (start,stop) = staticRangeOfStory story
  let allA = [ start .. stop - 1 ]
  let
    vm :: Map Addr Validity
    vm = Map.fromList [ (a, validOp_unfixed vf story a) | a <- allA ]
    vf :: Addr -> Validity
    vf addr = do
      if addr < start || addr >= stop then Invalid else
        maybe (error (show ("vf",addr))) id $ Map.lookup addr vm
  vf
  where
    validOp_unfixed :: (Addr -> Validity) -> Story -> Addr -> Validity
    validOp_unfixed vf story = checkA
      where
        checkA :: Addr -> Validity
        checkA a =
          case disOpM story a of
            Nothing -> Invalid
            Just (op,next) ->
              allV [ Valid (maxLocalRef op)
                   , checkAOA a op next
                   ]
        checkAOA :: Addr -> Operation -> Addr -> Validity
        checkAOA a op next = case op of
          Op.Jump a' -> callRecurse a'
          _ -> do
            case directCall op of
              Just rha -> do
                case disRoutineHeader story rha of
                  Nothing -> do
                    Invalid
                  Just (Op.BadRoutineHeader{},_) ->
                    error "BadRH impossible here"
                  Just (Op.RoutineHeader defs,routineStart) -> do
                    allV [ withFormalsCount (length defs)
                           (callRecurse routineStart)
                         , callRecurse next
                         ]
              Nothing -> do
                if isStopping op then Valid 0 else do
                  allV [ callRecurse a'
                       | a' <- [ next ] ++ branchesOf op
                       ]
          where
            callRecurse :: Addr -> Validity
            callRecurse a' =
              if a' > a
              then vf a'
              else vfLast a'


maxLocalRef :: Operation -> Byte
maxLocalRef op = case _opLocals op of
  [] -> 0
  xs -> maximum xs

_opLocals :: Operation -> [Byte]
_opLocals op = [ b | t <- opTargets op, Local b <- [t] ]


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
