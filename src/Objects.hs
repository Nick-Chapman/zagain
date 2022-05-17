
-- | Semantics of z-machine object-table operations.
module Objects
  ( getShortName
  , putProp
  , getProp
  , getPropAddr
  , getPropLen
  , getNextProp
  , testAttr
  , setAttr
  , clearAttr
  , insertObj
  , removeObj
  , getParent
  , getSibling
  , getChild
  ) where

import Control.Monad (when)
import Data.Bits (testBit,(.&.),shiftR,setBit,clearBit)
import Eff (Eff(..))
import Numbers (Byte,Addr,Value)
import Text.Printf (printf)

getShortName :: Int -> Eff String -- TODO: take Value instead of Int (everywhere)
getShortName o = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{propTable=PropTable{shortName}} = ob
  pure shortName

testAttr :: Int -> Int -> Eff Bool
testAttr o n = do
  zv <- getZversion
  base <- objectTableBase
  let f = formatOfVersion zv
  let maxAttNum = numByesForAttribute f * 8
  when (n >= maxAttNum) $ error (show ("attribute number too big",n))
  ob <- getObject base zv o
  let Object{atts=Attributes bools} = ob
  when (length bools /= maxAttNum) $ error "unexpected attribute bools"
  pure $ head (drop n bools)

setAttr :: Int -> Int -> Eff ()
setAttr o n = do
  zv <- getZversion
  base <- objectTableBase
  a <- objectAddr base zv o
  let d = n `div` 8
  let m = n `mod` 8
  let aa = a + fromIntegral d
  old <- GetByte aa
  let new = old `setBit` (7-m)
  SetByte aa new
  pure ()

clearAttr :: Int -> Int -> Eff ()
clearAttr o n = do
  zv <- getZversion
  base <- objectTableBase
  a <- objectAddr base zv o
  let d = n `div` 8
  let m = n `mod` 8
  let aa = a + fromIntegral d
  old <- GetByte aa
  let new = old `clearBit` (7-m)
  SetByte aa new
  pure ()

getProp :: Int -> Int -> Eff Value
getProp o n = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{propTable=PropTable{props}} = ob
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
putProp o n v = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{propTable=PropTable{props}} = ob
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
getPropAddr o n = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{propTable=PropTable{props}} = ob
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
getNextProp o p = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{propTable=PropTable{props}} = ob
  let bigger = [ n | Prop{number=n} <- props, p == 0 || n < p ]
  case bigger of
    [] -> pure 0
    _ -> pure $ maximum bigger

unlink :: Int -> Eff ()
unlink this = do
  oldP <- getParent this
  when (oldP /= 0) $ do
    child <- getChild oldP
    case child == this of
      True -> do
        thisSib <- getSibling this
        setChild oldP (byteOfInt thisSib)
      False -> do
        loop child
      where
        loop :: Int -> Eff ()
        loop x = do
          when (x == 0) $ error "unlink loop, failed to find unlinkee"
          sib <- getSibling x
          case  sib == this of
            False -> loop sib
            True -> do
              thisSib <- getSibling this
              setSibling x (byteOfInt thisSib)

insertObj :: Int -> Int -> Eff ()
insertObj o dest = do
  unlink o
  setParent o (byteOfInt dest)
  oldChild <- getChild dest
  setSibling o (byteOfInt oldChild)
  setChild dest (byteOfInt o)

removeObj :: Int -> Eff ()
removeObj o = do
  unlink o
  setParent o 0

byteOfInt :: Int -> Byte
byteOfInt i = do
  if i < 0 || i > 255 then error (show ("byteOfInt",i)) else fromIntegral i

setParent :: Int -> Byte -> Eff ()
setParent x p = do
  zv <- getZversion
  base <- objectTableBase
  a <- objectAddr base zv x
  SetByte (a+4) p

setSibling :: Int -> Byte -> Eff ()
setSibling x p = do
  zv <- getZversion
  base <- objectTableBase
  a <- objectAddr base zv x
  SetByte (a+5) p

setChild :: Int -> Byte -> Eff ()
setChild x p = do
  zv <- getZversion
  base <- objectTableBase
  a <- objectAddr base zv x
  SetByte (a+6) p

getParent :: Int -> Eff Int -- TODO: capture common pattern for parent/sibling/child
getParent o = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{parent} = ob
  pure parent

getSibling :: Int -> Eff Int
getSibling o = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{sibling} = ob
  pure sibling

getChild :: Int -> Eff Int
getChild o = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{child} = ob
  pure child

getZversion :: Eff Zversion
getZversion = versionOfByte <$> GetByte 0  -- TODO: share via header
  where
    versionOfByte = \case
      1 -> Z1
      2 -> Z2
      3 -> Z3
      4 -> Z4
      5 -> Z5
      n -> error (printf "unsupported z-machine version: %s" (show n))

objectTableBase :: Eff Addr
objectTableBase = getAddress 0xA -- TODO: share via header

data Object = Object -- TODO: remove
  { id :: Int
  , atts :: Attributes
  , parent :: Int
  , sibling :: Int
  , child :: Int
  , propTable :: PropTable
  }
  deriving Show

data Attributes = Attributes [Bool]

data PropTable = PropTable
  { shortName :: String
  , props :: [Prop]
  }
  deriving Show

data Prop = Prop { number :: Int, dataAddr :: Value, dataBytes :: [Byte] }
  deriving Show

instance Show Attributes where
  show (Attributes bs) = [ if b then '1' else '0' | b <- bs ]

getObject :: Addr -> Zversion -> Int -> Eff Object -- TODO: avoid calling this when implementing z-machine operations
getObject base zv id = do
  a <- objectAddr base zv id
  atts <- getAttributes zv a
  parent <- fromIntegral <$> GetByte (a+4) -- TODO: capture 4/5/6 from Relation
  sibling <- fromIntegral <$> GetByte (a+5)
  child <- fromIntegral <$> GetByte (a+6)
  a' <- getAddress (a+7)
  propTable <- getPropertyTable a'
  pure $ Object { id, atts, parent, sibling, child, propTable }

objectAddr :: Addr -> Zversion -> Int -> Eff Addr
objectAddr base zv o = do
  let f = formatOfVersion zv
  let objectEntrySize = numByesForAttribute f + (3 * objectIdSize f) + 2
  let propDefaultsSize = 2 * numProps f
  pure (base + fromIntegral (propDefaultsSize + (o-1) * objectEntrySize))

getAttributes :: Zversion -> Addr -> Eff Attributes
getAttributes zv a = do
  let f = formatOfVersion zv
  let n = numByesForAttribute f
  xs <- getBytes a n
  let bs = [ x `testBit` i | x <- xs, i <- reverse [0..7] ]
  pure $ Attributes bs

getPropertyTable :: Addr -> Eff PropTable
getPropertyTable a = do
  shortNameLen <- GetByte a
  shortName <- if shortNameLen == 0 then pure "" else GetText (a+1)
  props <- getProps (a + 1 + fromIntegral (2 * shortNameLen))
  pure $ PropTable {shortName,props}

getProps :: Addr -> Eff [Prop]
getProps a = do
  b <- GetByte a
  if b == 0 then pure [] else do
    let number = fromIntegral (b .&. 0x1f)
    let numBytes :: Int = 1 + fromIntegral (b `shiftR` 5)
    let dataAddr = fromIntegral (a + 1)
    dataBytes <- getBytes (a+1) numBytes
    let p1 = Prop {number,dataAddr,dataBytes}
    more <- getProps (a + fromIntegral (numBytes + 1))
    pure (p1:more)

getBytes :: Addr -> Int -> Eff [Byte]
getBytes a n = sequence [GetByte (a+i) | i <- [0..fromIntegral n - 1]]

getAddress :: Addr -> Eff Addr
getAddress a = do
  hi <- GetByte a
  lo <- GetByte (a+1)
  pure (256 * fromIntegral hi + fromIntegral lo)

data Zversion = Z1 | Z2 | Z3 | Z4 | Z5 -- TODO: move to new Header module
  deriving Show

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
