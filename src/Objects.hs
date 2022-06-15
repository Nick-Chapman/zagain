
-- | Semantics of z-machine object-table operations.
module Objects
  ( getShortName
  , testAttr, setAttr, clearAttr
  , FamilyMember(Parent,Sibling,Child), getFM
  , insertObj, removeObj
  , getProp, getPropAddr, putProp, getPropLen, getNextProp
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
              False -> loop sib -- TODO: infinite effect
              True -> do
                thisSib <- getFM Sibling this
                setFM Sibling x thisSib

--[properties]--------------------------------------------------------

getProp :: Phase p => Mode -> Value p -> Value p -> Eff p (Value p)
getProp mode x n = do
  Header{objectTable} <- StoryHeader
  searchProp mode x n >>= \case
    Nothing -> do
      -- get property default
      m1 <- LitV (-1)
      nM1 <- Add n m1
      two <- LitV 2
      off <- Mul two nM1
      base <- LitA objectTable
      a <- Offset base off
      getWord a
    Just Prop{dataAddr,numBytes} -> do
      two <- LitV 2
      Equal numBytes two >>= If >>= \case
        False -> -- If length not 2, then must be 1. But never seen this case.
          undefined -- GetByte dataAddr >>= Widen
        True -> do
          getWord dataAddr


getPropAddr :: Phase p => Mode -> Value p -> Value p -> Eff p (Addr p)
getPropAddr mode x n = do
  searchProp mode x n >>= \case
    Just Prop{dataAddr} -> pure dataAddr
    Nothing -> LitV 0 >>= Address


putProp :: Phase p => Mode -> Value p -> Value p -> Value p -> Eff p ()
putProp mode x n v = do
  searchProp mode x n >>= \case
    Nothing -> error (show ("putProp",n))
    Just Prop{dataAddr,numBytes} -> do
      two <- LitV 2
      Equal numBytes two >>= If >>= \case
        False -> do -- If length not 2, then must be 1. But never seen this case.
          _ <- undefined
          lo <- LoByte v
          SetByte dataAddr lo
        True -> do
          hi <- HiByte v
          lo <- LoByte v
          one <- LitV 1
          dataAddrPlusOne <- Offset dataAddr one
          SetByte dataAddr hi
          SetByte dataAddrPlusOne lo


getPropLen :: Phase p => Value p -> Eff p (Value p)
getPropLen v = do
  b <- IsZero v >>= If
  if b then LitV 0 else do
    one <- LitV 1
    vM1 <- Sub v one -- TODO: step back is more involved for Large (not always 1)
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
getNextProp mode x n = do
  IsZero n >>= If >>= \case
    True -> do
      a <- firstPropertyAddr x
      b <- GetByte a
      getPropNumber b
    False -> do
      searchProp mode x n >>= \case
        Nothing -> error (show ("getNextProp",n))
        Just Prop{dataAddr,numBytes} -> do
          a <- Offset dataAddr numBytes
          b <- GetByte a
          getPropNumber b


data Prop p = Prop
  { numBytes :: Value p
  , dataAddr :: Addr p
  }

deriving instance Phase p => Show (Prop p)

searchProp :: Phase p => Mode -> Value p -> Value p -> Eff p (Maybe (Prop p))
searchProp mode x n = do
  a1 <- firstPropertyAddr x
  case mode of
    Compiling -> Error "getPropsA" -- match name in regression code
    Interpreting -> do
      let
        loop a = do
          b <- GetByte a
          IsZeroByte b >>= If >>= \case
            True -> pure Nothing
            False -> do
              propNumber <- getPropNumber b
              PropSize{sizeByteSize,numBytes} <- getPropSize a b
              off <- LitV (fromIntegral sizeByteSize)
              dataAddr <- Offset a off
              let p1 = Prop {numBytes,dataAddr}
              b <- Equal n propNumber >>= If
              if b then pure (Just p1) else do
                Offset dataAddr numBytes >>= loop -- TODO: infinite effect
      loop a1


firstPropertyAddr :: Phase p => Value p -> Eff p (Addr p)
firstPropertyAddr x = do
  a <- propTableAddr x
  shortNameLen <- GetByte a
  size <- dubPlus1 shortNameLen
  Offset a size
  where
    dubPlus1 :: Phase p => Byte p -> Eff p (Value p)
    dubPlus1 b = do
      v <- Widen b
      one <- LitV 1
      two <- LitV 2
      dub <- Mul two v
      Add dub one


data PropSize p = PropSize
  { numBytes :: Value p
  , sizeByteSize :: Int
  }

getPropSize :: Phase p => Addr p -> Byte p -> Eff p (PropSize p)
getPropSize a1 b1 = do
  one <- LitV 1
  objectTableFormat >>= \case
    Small -> do
      numBytes <- do
        five <- LitV 5
        shifted <- b1 `ShiftR` five
        widened <- Widen shifted
        Add widened one
      pure PropSize{sizeByteSize=1,numBytes}
    Large -> do
      (LitB 7 >>= TestBit b1) >>= If >>= \case
        False -> do
          numBytes <- do
            b6 <- (LitB 6 >>= TestBit b1) >>= If
            LitV (case b6 of False -> 1; True -> 2)
          pure PropSize{sizeByteSize=1,numBytes}
        True -> do
          numBytes <- do
            a2 <- Offset a1 one
            b2 <- GetByte a2
            mask6 <- LitB 0x3f
            sixBits <- b2 `BwAnd` mask6
            Widen sixBits
          pure PropSize{sizeByteSize=2,numBytes}


getPropNumber :: Byte p -> Eff p (Value p)
getPropNumber b1 = do
  objectTableFormat >>= \case
    Small -> do
      oneF <- LitB 0x1f
      fiveBits <- b1 `BwAnd` oneF
      Widen fiveBits
    Large -> do
      mask6 <- LitB 0x3f
      sixBits <- b1 `BwAnd` mask6
      Widen sixBits

--[objectTableFormat]-------------------------------------------------

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
