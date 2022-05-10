
module Objects (dump) where

import Text.Printf (printf)
import Data.Bits (testBit,(.&.),shiftR)
import Numbers (Byte,Addr)
import Eff (Eff(..))

dump :: Eff ()
dump = do
  let os = [1..248]
  zv <- getZversion
  base <- objectTableBase
  _objects <- mapM (getObject base zv) os
  mapM_ Debug _objects
  pure ()

getZversion :: Eff Zversion
getZversion = versionOfByte <$> GetByte 0
  where
    versionOfByte = \case
      1 -> Z1
      2 -> Z2
      3 -> Z3
      4 -> Z4
      5 -> Z5
      n -> error (printf "unsupported z-machine version: %s" (show n))

objectTableBase :: Eff Addr
objectTableBase = getAddress 0xA

data Object = Object
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

data Prop = Prop { number :: Int, dataBytes :: [Byte] }
  deriving Show

instance Show Attributes where
  show (Attributes bs) = [ if b then '1' else '0' | b <- bs ]

getObject :: Addr -> Zversion -> Int -> Eff Object
getObject base zv id = do
  a <- objectAddr base zv id
  atts <- getAttributes zv a
  parent <- fromIntegral <$> GetByte (a+4)
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
    dataBytes <- getBytes (a+1) numBytes
    let p1 = Prop {number,dataBytes}
    more <- getProps (a + fromIntegral (numBytes + 1))
    pure (p1:more)

getBytes :: Addr -> Int -> Eff [Byte]
getBytes a n = sequence [GetByte (a+i) | i <- [0..fromIntegral n - 1]]

getAddress :: Addr -> Eff Addr
getAddress a = do
  hi <- GetByte a
  lo <- GetByte (a+1)
  pure (256 * fromIntegral hi + fromIntegral lo)

data Zversion = Z1 | Z2 | Z3 | Z4 | Z5
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
