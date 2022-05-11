
module Objects
  ( dump
  , getProp
  , insertObj
  , getParent
  , getSibling
  , getChild
  ) where

import Control.Monad (when)
import Text.Printf (printf)
import Data.Bits (testBit,(.&.),shiftR)
import Numbers (Byte,Addr,Value)
import Eff (Eff(..))

dump :: Eff ()
dump = do
  let os = [1..248]
  zv <- getZversion
  base <- objectTableBase
  objects <- mapM (getObject base zv) os
  mapM_ Debug objects
  pure ()

getProp :: Int -> Int -> Eff Value
getProp o n = do
  --Debug ("getProp",o,n)
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  --Debug ob
  let Object{propTable=PropTable{props}} = ob
  let xs = [ dataBytes | Prop{number,dataBytes} <- props, number == n ]
  let defaultValue = 0 -- TODO
  let x =
        case xs of
          [x] -> x
          [] -> [defaultValue]
          _ -> error "multi prop match"
  --Debug (x)
  case x of
    [hi,lo] -> pure (256 * fromIntegral hi + fromIntegral lo)
    [b] -> pure $ fromIntegral b
    _ -> error "expected 1 or 2 bytes for prop value"


insertObj :: Int -> Int -> Eff ()
insertObj o dest = do
  zv <- getZversion
  base <- objectTableBase
  assertWellFormed
  sizeBefore <- sizeObjectTree
  --Debug (sizeBefore)
  oldP <- getParent o
  if oldP /= 0 then error "need to unlimk" else do
    setParent base zv o (byteOfInt dest)
    oldChild <- getChild dest
    setSibling base zv o (byteOfInt oldChild)
    setChild base zv dest (byteOfInt o)
    assertWellFormed
    sizeAfter <- sizeObjectTree
    --Debug (sizeAfter)
    when (sizeAfter /= sizeBefore) $ error (show ("size of object tree has changed",sizeBefore,sizeAfter))

byteOfInt :: Int -> Byte
byteOfInt i = do
  if i < 0 || i > 255 then error (show ("byteOfInt",i)) else fromIntegral i

sizeObjectTree :: Eff Int
sizeObjectTree = do
  roots <- objectRoots
  forest <- mapM getTree roots
  pure $ sizeForest forest

objectRoots :: Eff [Int]
objectRoots = do
  let os = [1::Int ..248]
  ops <- sequence [ do p <- getParent o; pure (o,p) | o <- os ]
  pure [ o | (o,p) <- ops, p == 0 ]

data Tree = Tree Int [Tree] deriving Show

getTree :: Int -> Eff Tree
getTree x = do
  xs <- children x
  subs <- mapM getTree xs
  pure $ Tree x subs

sizeTree :: Tree -> Int
sizeTree (Tree _ subs) = 1 + sizeForest subs

sizeForest :: [Tree] -> Int
sizeForest xs = sum (map sizeTree xs)

_prettyForest :: [Tree] -> Eff ()
_prettyForest = mapM_ (prettyTree 0)
  where
    prettyTree i (Tree x subs) = do
      Debug (tab i ++ show x)
      mapM_ (prettyTree (i+2)) subs

    tab :: Int -> String
    tab n = take n (repeat ' ')


setParent :: Addr -> Zversion -> Int -> Byte -> Eff ()
setParent base zv x p = do
  a <- objectAddr base zv x
  SetByte (a+4) p

setSibling :: Addr -> Zversion -> Int -> Byte -> Eff ()
setSibling base zv x p = do
  a <- objectAddr base zv x
  SetByte (a+5) p

setChild :: Addr -> Zversion -> Int -> Byte -> Eff ()
setChild base zv x p = do
  a <- objectAddr base zv x
  SetByte (a+6) p


assertWellFormed :: Eff ()
assertWellFormed = do
  wf <- wellFormed
  if wf then pure () else error "not well formed"

wellFormed :: Eff Bool
wellFormed = do
  let os = [1::Int ..248]
  xs <- sequence [ do
                     cs <- children o
                     ps <- sequence [getParent c | c <- cs]
                     let ws = [ p == o | p <- ps ]
                     pure (o,cs,ps,ws,all Prelude.id ws)
                 | o <- os ]
  --mapM_ Debug xs
  let wf = all Prelude.id [ b | (_,_,_,_,b) <- xs ]
  pure wf

children :: Int -> Eff [Int]
children p = do
  getChild p >>= sibList

sibList :: Int -> Eff [Int]
sibList x = do
  if x == 0 then pure [] else do
    y <- getSibling x
    ys <- sibList y
    pure (x:ys)

getParent :: Int -> Eff Int
getParent o = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{parent} = ob
  pure parent

getChild :: Int -> Eff Int
getChild o = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{child} = ob
  pure child

getSibling :: Int -> Eff Int
getSibling o = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{sibling} = ob
  pure sibling



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
