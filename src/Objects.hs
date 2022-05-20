
-- | Semantics of z-machine object-table operations.
module Objects
  ( getShortName
  , testAttr, setAttr, clearAttr
  , FamilyMember(Parent,Sibling,Child), getFM
  , insertObj, removeObj
  , getProp, putProp, getPropAddr, getPropLen, getNextProp
  ) where

import Control.Monad (when)
import Data.Bits (testBit,(.&.),shiftR,setBit,clearBit)
import Eff (Eff(..))
import Header (Header(..),Zversion(..))
import Numbers (Byte,Addr,Value)

--[convs]-----------------------------------------------------

v2i :: Value -> Int
v2i = fromIntegral

v2a :: Value -> Addr
v2a = fromIntegral

v2b :: Value -> Byte
v2b = fromIntegral

b2v :: Byte -> Value
b2v = fromIntegral

bb2v :: Byte -> Byte -> Value
bb2v hi lo = 256 * b2v hi + b2v lo

getWord :: Addr -> Eff Value -- TODO: consider GetWord primitive
getWord a = do
  hi <- GetByte a
  lo <- GetByte (a+1)
  pure (bb2v hi lo)

objectAddr :: Value -> Eff Addr -- TODO: move static calcs to header?
objectAddr o = do
  Header{zv,objectTable} <- StoryHeader
  let base = objectTable
  let f = formatOfVersion zv
  let objectEntrySize = numByesForAttribute f + (3 * objectIdSize f) + 2
  let propDefaultsSize = 2 * numProps f
  pure (base + v2a (propDefaultsSize + (o-1) * objectEntrySize))

getShortName :: Value -> Eff String
getShortName x = do
  a <- objectAddr x
  a' <- getWord (a+7)
  shortNameLen <- GetByte (v2a a')
  if shortNameLen == 0 then pure "" else GetText (v2a (a'+1))

--[attributes]--------------------------------------------------------

testAttr :: Value -> Value -> Eff Bool
testAttr x n = do
  a <- objectAddr x
  let d = n `div` 8
  let m = n `mod` 8
  let aa = a + v2a d
  b <- GetByte aa
  pure $ b `testBit` v2i (7-m)

setAttr :: Value -> Value -> Eff ()
setAttr x n = do
  a <- objectAddr x
  let d = n `div` 8
  let m = n `mod` 8
  let aa = a + v2a d
  old <- GetByte aa
  let new = old `setBit` v2i (7-m)
  SetByte aa new

clearAttr :: Value -> Value -> Eff ()
clearAttr x n = do
  a <- objectAddr x
  let d = n `div` 8
  let m = n `mod` 8
  let aa = a + v2a d
  old <- GetByte aa
  let new = old `clearBit` v2i (7-m)
  SetByte aa new

--[object containment hierarchy]--------------------------------------

data FamilyMember = Parent | Sibling | Child

offsetFM :: FamilyMember -> Value
offsetFM = \case
  Parent -> 4
  Sibling -> 5
  Child -> 6

getFM :: FamilyMember -> Value -> Eff Value
getFM fm x = do
  a <- objectAddr x
  b2v <$> GetByte (a + v2a (offsetFM fm))

setFM :: FamilyMember -> Value -> Value -> Eff ()
setFM fm x y = do
  a <- objectAddr x
  SetByte (a + v2a (offsetFM fm)) (byteOfValue y)

byteOfValue :: Value -> Byte
byteOfValue v = do
  if v < 0 || v > 255 then error (show ("byteOfValue",v)) else v2b v


insertObj :: Value -> Value -> Eff ()
insertObj x dest = do
  unlink x
  setFM Parent x dest
  oldChild <- getFM Child dest
  setFM Sibling x oldChild
  setFM Child dest x

removeObj :: Value -> Eff ()
removeObj x = do
  unlink x
  setFM Parent x 0

unlink :: Value -> Eff ()
unlink this = do
  oldP <- getFM Parent this
  when (oldP /= 0) $ do
    child <- getFM Child oldP
    case child == this of
      True -> do
        thisSib <- getFM Sibling this
        setFM Child oldP thisSib
      False -> do
        loop child
      where
        loop :: Value -> Eff ()
        loop x = do
          when (x == 0) $ error "unlink loop, failed to find unlinkee"
          sib <- getFM Sibling x
          case  sib == this of
            False -> loop sib
            True -> do
              thisSib <- getFM Sibling this
              setFM Sibling x thisSib

--[properties]--------------------------------------------------------

getProp :: Value -> Value -> Eff Value
getProp x n = do
  Header{objectTable=base} <- StoryHeader
  props <- getPropertyTable x
  case [ dataBytes | Prop{number,dataBytes} <- props, number == n ] of
    [] -> do
      -- get property default
      getWord (base + v2a (2 * (n-1)))
    [prop] -> do
      case prop of
        [hi,lo] -> pure (bb2v hi lo)
        [b] -> pure $ b2v b
        _ -> error "expected 1 or 2 bytes for prop value"
    _ ->
      error "multi prop match"

putProp :: Value -> Value -> Value -> Eff ()
putProp x n v = do
  props <- getPropertyTable x
  let xs = [ pr | pr@Prop{number} <- props, number == n ]
  pr <-
    case xs of
      [x] -> pure x
      [] -> error "putProp, no such prop"
      _ -> error "multi prop match"

  let Prop{dataBytes,dataAddr=a} = pr
  case length dataBytes  of
    2 -> do
      let hi :: Byte = v2b (v `shiftR` 8)
      let lo :: Byte = v2b (v .&. 0xff)
      SetByte (v2a a) hi
      SetByte (v2a (a+1)) lo
      pure ()
    1 -> undefined
    _ -> error "expected 1 or 2 bytes for prop value"

getPropAddr :: Value -> Value -> Eff Value
getPropAddr x n = do
  props <- getPropertyTable x
  case [ dataAddr | Prop{number,dataAddr} <- props, number == n ] of
    [a] -> pure a
    [] -> pure 0
    _ -> error "multi prop match"

getPropLen :: Value -> Eff Value
getPropLen a = do
  if a == 0 then pure 0 else do
    b <- GetByte (v2a (a - 1))
    let numBytes = 1 + b2v ((b `shiftR` 5) .&. 0x7)
    pure numBytes

getNextProp :: Value -> Value -> Eff Value
getNextProp x p = do
  props <- getPropertyTable x
  let bigger = [ n | Prop{number=n} <- props, p == 0 || n < p ]
  case bigger of
    [] -> pure 0
    _ -> pure $ maximum bigger

getPropertyTable :: Value -> Eff [Prop]
getPropertyTable x = do
  a <- objectAddr x
  a' <- getWord (a+7)
  shortNameLen <- GetByte (v2a a')
  props <- getPropsA (a' + 1 + b2v (2 * shortNameLen))
  pure props

data Prop = Prop { number :: Value, dataAddr :: Value, dataBytes :: [Byte] }

getPropsA :: Value -> Eff [Prop]
getPropsA a = do
  b <- GetByte (v2a a)
  if b == 0 then pure [] else do
    let number = b2v (b .&. 0x1f)
    let numBytes = 1 + b2v (b `shiftR` 5)
    let dataAddr = a + 1
    dataBytes <- getBytes (a+1) numBytes
    let p1 = Prop {number,dataAddr,dataBytes}
    more <- getPropsA (a + numBytes + 1)
    pure (p1:more)

getBytes :: Value -> Value -> Eff [Byte]
getBytes a n = sequence [GetByte (v2a (a+i)) | i <- [0.. n - 1]]

--[odds and sods]-----------------------------------------------------

data ObjectTableFormat = Small | Large

formatOfVersion :: Zversion -> ObjectTableFormat
formatOfVersion = \case
  Z1 -> Small
  Z2 -> Small
  Z3 -> Small
  Z4 -> Large
  Z5 -> Large

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

