
-- | Semantics of z-machine object-table operations.
module Objects
  ( getShortName
  , testAttr, setAttr, clearAttr
  , FamilyMember(Parent,Sibling,Child), getFM
  , insertObj, removeObj
  , getProp, putProp, getPropAddr, getPropLen, getNextProp
  ) where

import Control.Monad (when)
import Eff (Eff(..),Phase(..))
import Header (Header(..),Zversion(..))
import qualified Numbers (Value)

--[convs]-----------------------------------------------------

getWord :: Addr p -> Eff p (Value p)
getWord a = do
  hi <- GetByte a
  one <- LitV 1
  a1 <- Offset a one
  lo <- GetByte a1
  MakeHiLo hi lo

objectAddr :: Value p -> Eff p (Addr p)
objectAddr o = do
  Header{zv,objectTable} <- StoryHeader
  let f = formatOfVersion zv
  let objectEntrySize = numByesForAttribute f + (3 * objectIdSize f) + 2
  size <- LitV objectEntrySize
  otBase <- LitA objectTable
  objectsOffset <- LitV (2 * numProps f - objectEntrySize)
  base <- Offset otBase objectsOffset
  off <- Mul o size
  Offset base off

getShortName :: Value p -> Eff p (Text p)
getShortName x = do
  base <- objectAddr x
  seven <- LitV 7
  a <- Offset base seven
  w <- getWord a
  a1 <- Address w
  shortNameLen <- GetByte a1
  p <- IsZeroByte shortNameLen >>= If
  if p then LitS "" else do
    one <- LitV 1
    w1 <- Add w one
    a2 <- Address w1
    GetText a2

--[attributes]--------------------------------------------------------

testAttr :: Value p -> Value p -> Eff p Bool
testAttr x n = do
  base <- objectAddr x
  d <- Div8 n
  m <- mod8 n
  m' <- SevenMinus m
  a <- Offset base d
  b <- GetByte a
  b `TestBit` m' >>= If

setAttr :: Value p -> Value p -> Eff p ()
setAttr x n = do
  base <- objectAddr x
  d <- Div8 n
  m <- mod8 n
  a <- Offset base d
  old <- GetByte a
  m' <- SevenMinus m
  new <- old `SetBit` m'
  SetByte a new

clearAttr :: Value p -> Value p -> Eff p ()
clearAttr x n = do
  base <- objectAddr x
  d <- Div8 n
  m <- mod8 n
  a <- Offset base d
  old <- GetByte a
  m' <- SevenMinus m
  new <- old `ClearBit` m'
  SetByte a new

mod8 :: Value p -> Eff p (Byte p)
mod8 v = do
  eight <- LitV 8
  res <- Mod v eight
  LoByte res

--[object containment hierarchy]--------------------------------------

data FamilyMember = Parent | Sibling | Child

offsetFM :: FamilyMember -> Numbers.Value
offsetFM = \case
  Parent -> 4
  Sibling -> 5
  Child -> 6

getFM :: FamilyMember -> Value p -> Eff p (Value p)
getFM fm x = do
  base <- objectAddr x
  off <- LitV (offsetFM fm)
  a <- Offset base off
  b <- GetByte a
  Widen b

setFM :: FamilyMember -> Value p -> Value p -> Eff p ()
setFM fm x y = do
  base <- objectAddr x
  lo <- LoByte y
  off <- LitV (offsetFM fm)
  a <- Offset base off
  SetByte a lo

insertObj :: Value p -> Value p -> Eff p ()
insertObj x dest = do
  unlink x
  setFM Parent x dest
  oldChild <- getFM Child dest
  setFM Sibling x oldChild
  setFM Child dest x

removeObj :: Value p -> Eff p ()
removeObj x = do
  unlink x
  zero <- LitV 0
  setFM Parent x zero

unlink :: forall p. Value p -> Eff p ()
unlink this = do
  oldP <- getFM Parent this
  b <- not <$> (IsZero oldP >>= If)
  when b $ do
    child <- getFM Child oldP
    b <- EqualAny [child,this] >>= If
    case b of
      True -> do
        thisSib <- getFM Sibling this
        setFM Child oldP thisSib
      False -> do
        loop child
      where
        loop :: Value p -> Eff p ()
        loop x = do
          b <- IsZero x >>= If
          if b then error "unlink loop, failed to find unlinkee" else do
            sib <- getFM Sibling x
            b <- EqualAny [sib,this] >>= If
            case b of
              False -> loop sib -- TODO: infinite effect is a problem for compilation
              True -> do
                thisSib <- getFM Sibling this
                setFM Sibling x thisSib

--[properties]--------------------------------------------------------

getPropN :: Value p -> Value p -> Eff p (Maybe (Prop p))
getPropN x n = do
  props <- getPropertyTable x
  xs <- sequence
    [ do b <- EqualAny [n,number] >>= If; pure (prop,b)
    | prop@Prop{number} <- props
    ]
  pure $ case [ prop | (prop,b) <- xs, b ] of
    [] -> Nothing
    [prop] -> Just prop
    _ -> error "getPropN: multi prop match"


getProp :: Value p -> Value p -> Eff p (Value p)
getProp x n = do
  Header{objectTable} <- StoryHeader

  getPropN x n >>= \case
    Nothing -> do
      -- get property default
      m1 <- LitV (-1)
      nM1 <- Add n m1
      two <- LitV 2
      off <- Mul two nM1
      base <- LitA objectTable
      a <- Offset base off
      getWord a
    Just (Prop{dataBytes}) -> do
      case dataBytes of
        [hi,lo] -> MakeHiLo hi lo
        [_b] -> undefined -- Widen _b -- not hit yet
        _ -> error "expected 1 or 2 bytes for prop value"

putProp :: Value p -> Value p -> Value p -> Eff p ()
putProp x n v = do
  pr <- do
    getPropN x n >>= \case
      Nothing -> Error "putProp, no such prop"
      Just x -> pure x

  let Prop{dataBytes,dataAddr} = pr
  case length dataBytes  of
    2 -> do
      hi <- HiByte v
      lo <- LoByte v
      one <- LitV 1
      dataAddrPlusOne <- Offset dataAddr one
      SetByte dataAddr hi
      SetByte dataAddrPlusOne lo
      pure ()
    1 -> Error "expected 2 bytes for a prop value"
    _ -> Error "expected 1 or 2 bytes for prop value"

getPropAddr :: Value p -> Value p -> Eff p (Value p)
getPropAddr x n = do
  getPropN x n >>= \case
    Just(Prop{dataAddr}) -> do DeAddress dataAddr
    Nothing -> LitV 0

getPropLen :: Value p -> Eff p (Value p)
getPropLen v = do
  b <- IsZero v >>= If
  if b then LitV 0 else do
    one <- LitV 1
    vM1 <- Sub v one
    aM1 <- Address vM1
    b <- GetByte aM1
    shifted <- b `ShiftR` 5
    seven <- LitB 0x7
    masked <- shifted `BwAnd` seven
    w <- Widen masked
    one <- LitV 1
    Add one w

getNextProp :: Value p -> Value p -> Eff p (Value p)
getNextProp x p = do
  -- TODO: stop being so complicated. Assume descendning order and avoid search
  props <- getPropertyTable x
  xs <-
    sequence
    [ do
        b1 <- IsZero p >>= If
        b2 <- LessThan n p >>= If
        pure (prop,b1,b2)
    | prop@Prop{number=n} <- props
    ]
  let bigger = [ n | (Prop{number=n},b1,b2) <- xs, b1 || b2 ]
  case bigger of
    [] -> LitV 0
    fst:_ -> pure fst -- assume the first is the biggest

getPropertyTable :: Value p -> Eff p [Prop p]
getPropertyTable x = do
  base <- objectAddr x
  seven <- LitV 0x7
  a <- Offset base seven
  a1 <- getWord a >>= Address
  shortNameLen <- GetByte a1
  size <- dubPlus1 shortNameLen
  a2 <- Offset a1 size
  props <- getPropsA a2
  pure props

dubPlus1 :: Byte p -> Eff p (Value p)
dubPlus1 b = do
  v <- Widen b
  one <- LitV 1
  two <- LitV 2
  dub <- Mul two v
  Add dub one


data Prop p = Prop -- TODO: using this type isn't very helpful
  { number :: Value p
  , dataAddr :: Addr p
  , dataBytes :: [Byte p]
  }

getPropsA :: Addr p -> Eff p [Prop p]
getPropsA a = do
  b <- GetByte a
  endOfProps <- IsZeroByte b >>= If
  if endOfProps then pure [] else do
    one <- LitV 1
    dataAddr <- Offset a one
    number <- do
      oneF <- LitB 0x1f
      fiveBits <- b `BwAnd` oneF
      Widen fiveBits
    numBytes <- do
      shifted <- b `ShiftR` 5
      widened <- Widen shifted
      Add widened one
    dataBytes <- getBytes dataAddr numBytes
    let p1 = Prop {number,dataAddr,dataBytes}
    a' <- Offset dataAddr numBytes
    more <- getPropsA a' -- TODO: infinite effect is a problem for compilation
    pure (p1:more)

getBytes :: Addr p -> Value p -> Eff p [Byte p]
getBytes a n = do
  stop <- IsZero n >>= If
  if stop then pure [] else do
    one <- LitV 1
    b <- GetByte a
    a' <- Offset a one
    n' <- Sub n one
    bs <- getBytes a' n'
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

numProps :: ObjectTableFormat -> Numbers.Value
numProps = \case
  Small -> 31
  Large -> 63

numByesForAttribute :: ObjectTableFormat -> Numbers.Value
numByesForAttribute = \case
  Small -> 4
  Large -> 6

objectIdSize :: ObjectTableFormat -> Numbers.Value
objectIdSize = \case
  Small -> 1
  Large -> 2

