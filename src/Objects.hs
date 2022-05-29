
-- | Semantics of z-machine object-table operations.
module Objects
  ( getShortName
  , testAttr, setAttr, clearAttr
  , FamilyMember(Parent,Sibling,Child), getFM
  , insertObj, removeObj
  , getProp, putProp, getPropAddr, getPropLen, getNextProp
  ) where

import Control.Monad (when)
import Eff (Eff(..),Bin(..))
import Header (Header(..),Zversion(..))
import Numbers (Value)

--[convs]-----------------------------------------------------

getWord :: a -> Eff a b s v v
getWord a = do
  hi <- GetByte a
  one <- LitV 1
  a1 <- Offset a one
  lo <- GetByte a1
  MakeWord hi lo

objectAddr :: v -> Eff a b s v a
objectAddr o = do
  Header{zv,objectTable} <- StoryHeader
  let f = formatOfVersion zv
  let objectEntrySize = numByesForAttribute f + (3 * objectIdSize f) + 2
  size <- LitV objectEntrySize
  otBase <- LitA objectTable
  objectsOffset <- LitV (2 * numProps f - objectEntrySize)
  base <- Offset otBase objectsOffset
  off <- BinOp BMul o size
  Offset base off

getShortName :: v -> Eff a b s v s
getShortName x = do
  base <- objectAddr x
  seven <- LitV 7
  a <- Offset base seven
  w <- getWord a
  a1 <- Address w
  shortNameLen <- GetByte a1
  p <- IsZeroByte shortNameLen
  if p then LitS "" else do
    one <- LitV 1
    w1 <- BinOp BAdd w one
    a2 <- Address w1
    GetText a2

--[attributes]--------------------------------------------------------

testAttr :: v -> v -> Eff a b s v Bool
testAttr x n = do
  base <- objectAddr x
  d <- Div8 n
  m <- Mod8 n
  m' <- SevenMinus m
  a <- Offset base d
  b <- GetByte a
  b `TestBit` m'

setAttr :: v -> v -> Eff a b s v ()
setAttr x n = do
  base <- objectAddr x
  d <- Div8 n
  m <- Mod8 n
  a <- Offset base d
  old <- GetByte a
  m' <- SevenMinus m
  new <- old `SetBit` m'
  SetByte a new

clearAttr :: v -> v -> Eff a b s v ()
clearAttr x n = do
  base <- objectAddr x
  d <- Div8 n
  m <- Mod8 n
  a <- Offset base d
  old <- GetByte a
  m' <- SevenMinus m
  new <- old `ClearBit` m'
  SetByte a new

--[object containment hierarchy]--------------------------------------

data FamilyMember = Parent | Sibling | Child

offsetFM :: FamilyMember -> Value
offsetFM = \case
  Parent -> 4
  Sibling -> 5
  Child -> 6

getFM :: FamilyMember -> v -> Eff a b s v v
getFM fm x = do
  base <- objectAddr x
  off <- LitV (offsetFM fm)
  a <- Offset base off
  b <- GetByte a
  Widen b

setFM :: FamilyMember -> v -> v -> Eff a b s v ()
setFM fm x y = do
  base <- objectAddr x
  lo <- LoByte y
  off <- LitV (offsetFM fm)
  a <- Offset base off
  SetByte a lo

insertObj :: v -> v -> Eff a b s v ()
insertObj x dest = do
  unlink x
  setFM Parent x dest
  oldChild <- getFM Child dest
  setFM Sibling x oldChild
  setFM Child dest x

removeObj :: v -> Eff a b s v ()
removeObj x = do
  unlink x
  zero <- LitV 0
  setFM Parent x zero

unlink :: forall a b s v. v -> Eff a b s v ()
unlink this = do
  oldP <- getFM Parent this
  b <- not <$> IsZero oldP
  when b $ do
    child <- getFM Child oldP
    b <- EqualAny [child,this]
    case b of
      True -> do
        thisSib <- getFM Sibling this
        setFM Child oldP thisSib
      False -> do
        loop child
      where
        loop :: v -> Eff a b s v ()
        loop x = do
          b <- IsZero x
          if b then error "unlink loop, failed to find unlinkee" else do
            sib <- getFM Sibling x
            b <- EqualAny [sib,this]
            case b of
              False -> loop sib
              True -> do
                thisSib <- getFM Sibling this
                setFM Sibling x thisSib

--[properties]--------------------------------------------------------

getPropN :: v -> v -> Eff a b s v (Maybe (Prop b v))
getPropN x n = do
  props <- getPropertyTable x
  xs <- sequence
    [ do b <- EqualAny [n,number]; pure (prop,b)
    | prop@Prop{number} <- props
    ]
  pure $ case [ prop | (prop,b) <- xs, b ] of
    [] -> Nothing
    [prop] -> Just prop
    _ -> error "getPropN: multi prop match"


getProp :: v -> v -> Eff a b s v v
getProp x n = do
  Header{objectTable} <- StoryHeader

  getPropN x n >>= \case
    Nothing -> do
      -- get property default
      m1 <- LitV (-1)
      nM1 <- BinOp BAdd n m1
      two <- LitV 2
      off <- BinOp BMul two nM1
      base <- LitA objectTable
      a <- Offset base off
      getWord a
    Just (Prop{dataBytes}) -> do
      case dataBytes of
        [hi,lo] -> MakeWord hi lo
        [b] -> undefined (Widen b) -- not hit yet
        _ -> error "expected 1 or 2 bytes for prop value"

putProp :: Show b => v -> v -> v -> Eff a b s v ()
putProp x n v = do
  pr <- do
    getPropN x n >>= \case
      Nothing -> error "putProp, no such prop"
      Just x -> pure x

  let Prop{dataBytes,dataAddr} = pr
  case length dataBytes  of
    2 -> do
      hi <- HiByte v
      lo <- LoByte v
      a0 <- Address dataAddr
      one <- LitV 1
      a1 <- Offset a0 one
      SetByte a0 hi
      SetByte a1 lo
      pure ()
    1 -> undefined
    _ -> error "expected 1 or 2 bytes for prop value"

getPropAddr :: v -> v -> Eff a b s v v
getPropAddr x n = do
  getPropN x n >>= \case
    Just(Prop{dataAddr}) -> pure dataAddr
    Nothing -> LitV 0

getPropLen :: v -> Eff a b s v v
getPropLen v = do
  b <- IsZero v
  if b then LitV 0 else do
    one <- LitV 1
    vM1 <- BinOp BSub v one
    aM1 <- Address vM1
    b <- GetByte aM1
    shifted <- b `ShiftR` 5
    seven <- LitB 0x7
    masked <- shifted `BwAnd` seven
    w <- Widen masked
    one <- LitV 1
    BinOp BAdd one w

getNextProp :: v -> v -> Eff a b s v v
getNextProp x p = do
  -- TODO: stop being so complicated. Assume descendning order and avoid search
  props <- getPropertyTable x
  xs <-
    sequence
    [ do
        b1 <- IsZero p
        b2 <- LessThan n p
        pure (prop,b1,b2)
    | prop@Prop{number=n} <- props
    ]
  let bigger = [ n | (Prop{number=n},b1,b2) <- xs, b1 || b2 ]
  case bigger of
    [] -> LitV 0
    fst:_ -> pure fst -- assume the first is the biggest

getPropertyTable :: v -> Eff a b s v [Prop b v]
getPropertyTable x = do
  base <- objectAddr x
  seven <- LitV 0x7
  a <- Offset base seven
  v <- getWord a
  a1 <- Address v
  shortNameLen <- GetByte a1
  off <- dubPlus1 shortNameLen
  v' <- BinOp BAdd v off
  props <- getPropsA v'
  takeWhileDescending props

dubPlus1 :: b -> Eff a b s v v
dubPlus1 b = do --Widen (1 + 2 * shortNameLen)
  v <- Widen b
  one <- LitV 1
  two <- LitV 2
  dub <- BinOp BMul two v
  BinOp BAdd dub one


takeWhileDescending :: [Prop b v] -> Eff a b s v [Prop b v]
takeWhileDescending = \case
  [] -> pure []
  p@Prop{number}:ps -> do
    ps <- loop number ps
    pure (p : ps)
    where
      loop :: v -> [Prop b v] -> Eff a b s v [Prop b v]
      loop last = \case
        [] -> pure []
        p@Prop{number}:ps -> do
          b <- LessThan number last
          if not b then pure [] else do
            ps <- loop number ps
            pure (p : ps)


data Prop b v = Prop -- TODO: using this type is not helpful
  { number :: v
  , dataAddr :: v -- TODO: dataAdd should be Addr!
  , dataBytes :: [b]
  } deriving Show

getPropsA :: v -> Eff a b s v [Prop b v] -- TODO: first arg should be address
getPropsA v = do
  a <- Address v
  b <- GetByte a
  p <- IsZeroByte b
  if p then pure [] else do
    oneF <- LitB 0x1f
    fiveBits <- b `BwAnd` oneF
    number <- Widen fiveBits
    shifted <- b `ShiftR` 5
    widened <- Widen shifted
    one <- LitV 1
    numBytes <- BinOp BAdd widened one
    dataAddr <- BinOp BAdd v one
    dataBytes <- getBytes dataAddr numBytes
    let p1 = Prop {number,dataAddr,dataBytes}
    v' <- BinOp BAdd v numBytes
    v'' <- BinOp BAdd v' one
    more <- getPropsA v'' -- TODO: loop
    pure (p1:more)

getBytes :: v -> v -> Eff a b s v [b]
getBytes v n = do
  a <- Address v
  getBytesA a n

getBytesA :: a -> v -> Eff a b s v [b]
getBytesA a n = do
  stop <- IsZero n
  if stop then pure [] else do
    one <- LitV 1
    b <- GetByte a
    a' <- Offset a one
    n' <- BinOp BSub n one
    bs <- getBytesA a' n'
    pure (b:bs)


--[odds and sods]-----------------------------------------------------

data ObjectTableFormat = Small | Large

formatOfVersion :: Zversion -> ObjectTableFormat
formatOfVersion = \case
  Z1 -> Small
  Z2 -> Small
  Z3 -> Small
  Z4 -> Large
  Z5 -> Large
  Z6 -> Large

numProps :: ObjectTableFormat -> Value
numProps = \case
  Small -> 31
  Large -> 63

numByesForAttribute :: ObjectTableFormat -> Value
numByesForAttribute = \case
  Small -> 4
  Large -> 6

objectIdSize :: ObjectTableFormat -> Value
objectIdSize = \case
  Small -> 1
  Large -> 2

