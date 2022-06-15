
-- | Semantics of z-machine object-table operations.
module Objects
  ( getShortName
  , testAttr, setAttr, clearAttr
  , FamilyMember(Parent,Sibling,Child), getFM
  , insertObj, removeObj
  , getProp, putProp, getPropAddr, getPropLen, getNextProp
  ) where

import Control.Monad (when)
import Eff (Eff(..),Phase(..),Mode(..))
import Header (Header(..))
import Numbers (Zversion(..))
import qualified Numbers (Value)

--[convs]-----------------------------------------------------

getWord :: Phase p => Addr p -> Eff p (Value p)
getWord a = do
  hi <- GetByte a
  one <- LitV 1
  a1 <- Offset a one
  lo <- GetByte a1
  MakeHiLo hi lo

objectAddr :: Phase p => Value p -> Eff p (Addr p)
objectAddr o = do
  Header{objectTable} <- StoryHeader
  f <- objectTableFormat
  let objectEntrySize = numByesForAttribute f + (3 * objectIdSize f) + 2
  size <- LitV objectEntrySize
  otBase <- LitA objectTable
  objectsOffset <- LitV (2 * numProps f - objectEntrySize)
  base <- Offset otBase objectsOffset
  off <- Mul o size
  Offset base off

propTableAddr :: Phase p => Value p -> Eff p (Addr p)
propTableAddr x = do
  base <- objectAddr x
  f <- objectTableFormat
  off <- LitV (numByesForAttribute f + (3 * objectIdSize f))
  a <- Offset base off
  getWord a >>= Address

getShortName :: Phase p => Value p -> Eff p (Text p)
getShortName x = do
  a1 <- propTableAddr x
  shortNameLen <- GetByte a1
  p <- IsZeroByte shortNameLen >>= If
  if p then LitS "" else do
    one <- LitV 1
    a2 <- Offset a1 one
    GetText a2

--[attributes]--------------------------------------------------------

testAttr :: Phase p => Value p -> Value p -> Eff p Bool
testAttr x n = do
  base <- objectAddr x
  d <- Div8 n
  m <- mod8 n
  m' <- SevenMinus m
  a <- Offset base d
  b <- GetByte a
  b `TestBit` m' >>= If

setAttr :: Phase p => Value p -> Value p -> Eff p ()
setAttr x n = do
  base <- objectAddr x
  d <- Div8 n
  m <- mod8 n
  a <- Offset base d
  old <- GetByte a
  m' <- SevenMinus m
  new <- old `SetBit` m'
  SetByte a new

clearAttr :: Phase p => Value p -> Value p -> Eff p ()
clearAttr x n = do
  base <- objectAddr x
  d <- Div8 n
  m <- mod8 n
  a <- Offset base d
  old <- GetByte a
  m' <- SevenMinus m
  new <- old `ClearBit` m'
  SetByte a new

mod8 :: Phase p => Value p -> Eff p (Byte p)
mod8 v = do
  eight <- LitV 8
  res <- Mod v eight
  LoByte res

--[object containment hierarchy]--------------------------------------

data FamilyMember = Parent | Sibling | Child deriving Show

indexFM :: FamilyMember -> Numbers.Value
indexFM = \case
  Parent -> 0
  Sibling -> 1
  Child -> 2

offsetFM :: Phase p => FamilyMember -> Eff p (Value p)
offsetFM fm = do
  f <- objectTableFormat
  LitV (numByesForAttribute f + (indexFM fm * objectIdSize f))

getFM :: Phase p => FamilyMember -> Value p -> Eff p (Value p)
getFM fm x = do
  base <- objectAddr x
  off <- offsetFM fm
  a <- Offset base off
  objectTableFormat >>= \case
    Small -> do
      b <- GetByte a
      Widen b
    Large -> do
      getWord a

setFM :: Phase p => FamilyMember -> Value p -> Value p -> Eff p ()
setFM fm x y = do
  base <- objectAddr x
  off <- offsetFM fm
  a <- Offset base off
  objectTableFormat >>= \case
    Small -> do
      lo <- LoByte y
      SetByte a lo
    Large -> do
      hi <- HiByte y
      SetByte a hi
      one <- LitV 1
      a1 <- Offset a one
      lo <- LoByte y
      SetByte a1 lo

insertObj :: Phase p => Mode -> Value p -> Value p -> Eff p ()
insertObj mode x dest = do
  unlink mode x
  setFM Parent x dest
  oldChild <- getFM Child dest
  setFM Sibling x oldChild
  setFM Child dest x

removeObj :: Phase p => Mode -> Value p -> Eff p ()
removeObj mode x = do
  unlink mode x
  zero <- LitV 0
  setFM Parent x zero

unlink :: Phase p => Mode -> Value p -> Eff p ()
unlink mode this = do
  oldP <- getFM Parent this
  b <- not <$> (IsZero oldP >>= If)
  when b $ do
    child <- getFM Child oldP
    b <- Equal child this >>= If
    case b of
      True -> do
        thisSib <- getFM Sibling this
        setFM Child oldP thisSib
      False -> do
        case mode of
          Compiling -> Error "unlink/loop"
          Interpreting -> loop child
      where
        loop x = do
          b <- IsZero x >>= If
          if b then error "unlink loop, failed to find unlinkee" else do
            sib <- getFM Sibling x
            b <- Equal sib this >>= If
            case b of
              False -> loop sib -- TODO: infinite effect is a problem for compilation
              True -> do
                thisSib <- getFM Sibling this
                setFM Sibling x thisSib

--[properties]--------------------------------------------------------

getPropN :: forall p. Phase p => Mode -> Value p -> Value p -> Eff p (Maybe (Prop p))
getPropN mode x n = do
  a1 <- propTableAddr x
  shortNameLen <- GetByte a1
  size <- dubPlus1 shortNameLen
  a2 <- Offset a1 size
  case mode of
    Compiling -> Error "getPropsA" -- match name in regression code
    Interpreting -> do
      let
        loop :: Phase p => Addr p -> Eff p (Maybe (Prop p))
        loop a1 = do
          b1 <- GetByte a1
          endOfProps <- IsZeroByte b1 >>= If
          if endOfProps then pure Nothing else do
            (p1,a') <- getOneProp a1 b1
            let Prop{propNumber} = p1
            b <- Equal n propNumber >>= If
            if b then pure (Just p1) else do
              loop a' -- TODO: infinite effect
      loop a2

getOneProp :: Phase p => Addr p -> Byte p -> Eff p (Prop p, Addr p)
getOneProp a1 b1 = do
    PropNumAndSize{sizeByteSize,propNumber,numBytes} <- getPropNumAndSize a1 b1
    off <- LitV (fromIntegral sizeByteSize)
    dataAddr <- Offset a1 off
    dataBytes <- getBytes dataAddr numBytes
    let p1 = Prop {propNumber,numBytes,dataAddr,dataBytes}
    a' <- Offset dataAddr numBytes
    pure (p1,a')

----------------------------------------------------------------------

getProp :: Phase p => Mode -> Value p -> Value p -> Eff p (Value p)
getProp mode x n = do
  Header{objectTable} <- StoryHeader

  getPropN mode x n >>= \case
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

putProp :: Phase p => Mode -> Value p -> Value p -> Value p -> Eff p ()
putProp mode x n v = do
  pr <- do
    getPropN mode x n >>= \case
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

getPropAddr :: Phase p => Mode -> Value p -> Value p -> Eff p (Value p)
getPropAddr mode x n = do
  getPropN mode x n >>= \case
    Just(Prop{dataAddr}) -> do DeAddress dataAddr
    Nothing -> LitV 0

getPropLen :: Phase p => Value p -> Eff p (Value p)
getPropLen v = do
  b <- IsZero v >>= If
  if b then LitV 0 else do
    one <- LitV 1
    vM1 <- Sub v one
    aM1 <- Address vM1
    b <- GetByte aM1
    five <- LitV 5
    shifted <- b `ShiftR` five
    seven <- LitB 0x7
    masked <- shifted `BwAnd` seven
    w <- Widen masked
    one <- LitV 1
    Add one w

getNextProp :: Phase p => Mode -> Value p -> Value p -> Eff p (Value p)
getNextProp mode x p = do
  -- TODO: stop being so complicated. Assume descending order and avoid search
  props <- getPropertyTable mode x
  xs <-
    sequence
    [ do
        b1 <- IsZero p >>= If
        b2 <- LessThan n p >>= If
        pure (prop,b1,b2)
    | prop@Prop{propNumber=n} <- props
    ]
  let bigger = [ n | (Prop{propNumber=n},b1,b2) <- xs, b1 || b2 ]
  case bigger of
    [] -> LitV 0
    fst:_ -> pure fst -- assume the first is the biggest

getPropertyTable :: Phase p => Mode -> Value p -> Eff p [Prop p]
getPropertyTable mode x = do
  a1 <- propTableAddr x
  shortNameLen <- GetByte a1
  size <- dubPlus1 shortNameLen
  a2 <- Offset a1 size
  props <- getPropsA mode a2
  --Debug ("getPropertyTable",x,"...")
  --mapM_ Debug props
  pure props

dubPlus1 :: Phase p => Byte p -> Eff p (Value p)
dubPlus1 b = do
  v <- Widen b
  one <- LitV 1
  two <- LitV 2
  dub <- Mul two v
  Add dub one

data Prop p = Prop -- TODO: using this type isn't very helpful
  { propNumber :: Value p
  , numBytes :: Value p
  , dataAddr :: Addr p
  , dataBytes :: [Byte p]
  }

deriving instance Phase p => Show (Prop p)

getPropsA :: Phase p => Mode -> Addr p -> Eff p [Prop p]
getPropsA = \case
  Compiling -> hack_getPropsA
  Interpreting -> real_getPropsA

hack_getPropsA :: Phase p => Addr p -> Eff p [Prop p]
hack_getPropsA _a = Error "getPropsA" -- TODO: need while-effect!

real_getPropsA :: Phase p => Addr p -> Eff p [Prop p]
real_getPropsA a1 = do
  b1 <- GetByte a1
  endOfProps <- IsZeroByte b1 >>= If
  if endOfProps then pure [] else do
    PropNumAndSize{sizeByteSize,propNumber,numBytes} <- getPropNumAndSize a1 b1
    off <- LitV (fromIntegral sizeByteSize)
    dataAddr <- Offset a1 off
    dataBytes <- getBytes dataAddr numBytes
    let p1 = Prop {propNumber,numBytes,dataAddr,dataBytes}
    a' <- Offset dataAddr numBytes
    more <- real_getPropsA a' -- TODO: infinite effect is a problem for compilation
    pure (p1:more)

data PropNumAndSize p = PropNumAndSize
  { propNumber :: Value p
  , numBytes :: Value p
  , sizeByteSize :: Int
  }

getPropNumAndSize :: Phase p => Addr p -> Byte p -> Eff p (PropNumAndSize p)
getPropNumAndSize a1 b1 = do
  one <- LitV 1
  objectTableFormat >>= \case
    Small -> do
      propNumber <- do
        oneF <- LitB 0x1f
        fiveBits <- b1 `BwAnd` oneF
        Widen fiveBits
      numBytes <- do
        five <- LitV 5
        shifted <- b1 `ShiftR` five
        widened <- Widen shifted
        Add widened one
      pure PropNumAndSize{sizeByteSize=1,propNumber,numBytes}
    Large -> do
      mask6 <- LitB 0x3f
      propNumber <- do
        sixBits <- b1 `BwAnd` mask6
        Widen sixBits
      (LitB 7 >>= TestBit b1) >>= If >>= \case
        False -> do
          numBytes <- do
            b6 <- (LitB 6 >>= TestBit b1) >>= If
            LitV (case b6 of False -> 1; True -> 2)
          pure PropNumAndSize{sizeByteSize=1,propNumber,numBytes}
        True -> do
          numBytes <- do
            a2 <- Offset a1 one
            b2 <- GetByte a2
            sixBits <- b2 `BwAnd` mask6
            Widen sixBits
          pure PropNumAndSize{sizeByteSize=2,propNumber,numBytes}

getBytes :: Phase p => Addr p -> Value p -> Eff p [Byte p]
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

objectTableFormat :: Eff p ObjectTableFormat
objectTableFormat = do
  Header{zv} <- StoryHeader
  pure $ formatOfVersion zv

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

