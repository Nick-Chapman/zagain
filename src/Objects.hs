
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


objectAddr :: Int -> Eff Addr -- TODO: move static calcs to header?
objectAddr o = do
  Header{zv,objectTable=base} <- StoryHeader
  let f = formatOfVersion zv
  let objectEntrySize = numByesForAttribute f + (3 * objectIdSize f) + 2
  let propDefaultsSize = 2 * numProps f
  pure (base + fromIntegral (propDefaultsSize + (o-1) * objectEntrySize))

getShortName :: Int -> Eff String -- TODO: take Value instead of Int (everywhere)
getShortName x = do
  a <- objectAddr x
  a' <- getAddress (a+7)
  shortNameLen <- GetByte a'
  if shortNameLen == 0 then pure "" else GetText (a'+1)

--[attributes]--------------------------------------------------------

testAttr :: Int -> Int -> Eff Bool
testAttr x n = do
  a <- objectAddr x
  let d = n `div` 8
  let m = n `mod` 8
  let aa = a + fromIntegral d
  b <- GetByte aa
  pure $ b `testBit` (7-m)

setAttr :: Int -> Int -> Eff ()
setAttr x n = do
  a <- objectAddr x
  let d = n `div` 8
  let m = n `mod` 8
  let aa = a + fromIntegral d
  old <- GetByte aa
  let new = old `setBit` (7-m)
  SetByte aa new

clearAttr :: Int -> Int -> Eff ()
clearAttr x n = do
  a <- objectAddr x
  let d = n `div` 8
  let m = n `mod` 8
  let aa = a + fromIntegral d
  old <- GetByte aa
  let new = old `clearBit` (7-m)
  SetByte aa new

--[object containment hierarchy]--------------------------------------

data FamilyMember = Parent | Sibling | Child

offsetFM :: FamilyMember -> Int
offsetFM = \case
  Parent -> 4
  Sibling -> 5
  Child -> 6

getFM :: FamilyMember -> Int -> Eff Int
getFM fm x = do
  a <- objectAddr x
  fromIntegral <$> GetByte (a + fromIntegral (offsetFM fm))

setFM :: FamilyMember -> Int -> Int -> Eff ()
setFM fm x y = do
  a <- objectAddr x
  SetByte (a + fromIntegral (offsetFM fm)) (byteOfInt y)

byteOfInt :: Int -> Byte
byteOfInt i = do
  if i < 0 || i > 255 then error (show ("byteOfInt",i)) else fromIntegral i


insertObj :: Int -> Int -> Eff ()
insertObj x dest = do
  unlink x
  setFM Parent x dest
  oldChild <- getFM Child dest
  setFM Sibling x oldChild
  setFM Child dest x

removeObj :: Int -> Eff ()
removeObj x = do
  unlink x
  setFM Parent x 0

unlink :: Int -> Eff ()
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
        loop :: Int -> Eff ()
        loop x = do
          when (x == 0) $ error "unlink loop, failed to find unlinkee"
          sib <- getFM Sibling x
          case  sib == this of
            False -> loop sib
            True -> do
              thisSib <- getFM Sibling this
              setFM Sibling x thisSib

--[properties]--------------------------------------------------------

getProp :: Int -> Int -> Eff Value
getProp x n = do
  Header{objectTable=base} <- StoryHeader
  props <- getPropertyTable x
  let xs = [ dataBytes | Prop{number,dataBytes} <- props, number == n ]
  x <-
    case xs of
      [x] -> pure x
      [] -> do
        -- get property default
        hi <- GetByte (base + fromIntegral (2 * (n-1)))
        lo <- GetByte (base + fromIntegral (2 * (n-1) + 1))
        pure [hi,lo]
      _ ->
        error "multi prop match"
  case x of
    [hi,lo] -> pure (256 * fromIntegral hi + fromIntegral lo)
    [b] -> pure $ fromIntegral b
    _ -> error "expected 1 or 2 bytes for prop value"

putProp :: Int -> Int -> Value -> Eff ()
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
      let hi :: Byte = fromIntegral (v `shiftR` 8)
      let lo :: Byte = fromIntegral (v .&. 0xff)
      SetByte (fromIntegral a) hi
      SetByte (fromIntegral (a+1)) lo
      pure ()
    1 -> undefined
    _ -> error "expected 1 or 2 bytes for prop value"

getPropAddr :: Int -> Int -> Eff Value
getPropAddr x n = do
  props <- getPropertyTable x
  case [ dataAddr | Prop{number,dataAddr} <- props, number == n ] of
    [a] -> pure a
    [] -> pure 0
    _ -> error "multi prop match"

getPropLen :: Value -> Eff Value
getPropLen a = do
  if a == 0 then pure 0 else do
    b <- GetByte (fromIntegral a - 1)
    let numBytes :: Value = 1 + fromIntegral ((b `shiftR` 5) .&. 0x7)
    pure numBytes

getNextProp :: Int -> Int -> Eff Int
getNextProp x p = do
  props <- getPropertyTable x
  let bigger = [ n | Prop{number=n} <- props, p == 0 || n < p ]
  case bigger of
    [] -> pure 0
    _ -> pure $ maximum bigger

getPropertyTable :: Int -> Eff [Prop]
getPropertyTable x = do
  a <- objectAddr x
  a' <- getAddress (a+7)
  shortNameLen <- GetByte a'
  props <- getPropsA (a' + 1 + fromIntegral (2 * shortNameLen))
  pure props

data Prop = Prop { number :: Int, dataAddr :: Value, dataBytes :: [Byte] }

getPropsA :: Addr -> Eff [Prop]
getPropsA a = do
  b <- GetByte a
  if b == 0 then pure [] else do
    let number = fromIntegral (b .&. 0x1f)
    let numBytes :: Int = 1 + fromIntegral (b `shiftR` 5)
    let dataAddr = fromIntegral (a + 1)
    dataBytes <- getBytes (a+1) numBytes
    let p1 = Prop {number,dataAddr,dataBytes}
    more <- getPropsA (a + fromIntegral (numBytes + 1))
    pure (p1:more)

--[odds and sods]-----------------------------------------------------

getBytes :: Addr -> Int -> Eff [Byte]
getBytes a n = sequence [GetByte (a+i) | i <- [0..fromIntegral n - 1]]

getAddress :: Addr -> Eff Addr -- TODO: consider GetWord primitive
getAddress a = do
  hi <- GetByte a
  lo <- GetByte (a+1)
  pure (256 * fromIntegral hi + fromIntegral lo)

data ObjectTableFormat = Small | Large

formatOfVersion :: Zversion -> ObjectTableFormat
formatOfVersion = \case
  Z1 -> Small
  Z2 -> Small
  Z3 -> Small
  Z4 -> Large
  Z5 -> Large

numProps :: ObjectTableFormat -> Int
numProps = \case
  Small -> 31
  Large -> 63

numByesForAttribute :: ObjectTableFormat -> Int
numByesForAttribute = \case
  Small -> 4
  Large -> 6

objectIdSize :: ObjectTableFormat -> Int
objectIdSize = \case
  Small -> 1
  Large -> 2
