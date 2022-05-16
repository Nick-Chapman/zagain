
module Objects
  ( dump
  , getShortName
  , putProp
  , getProp
  , getPropAddr
  , getPropLen
  , testAttr
  , setAttr
  , clearAttr
  , insertObj
  , getParent
  , getSibling
  , getChild
  ) where

import Control.Monad (when)
import Data.Bits (testBit,(.&.),shiftR,setBit,clearBit)
import Eff (Eff(..))
import Numbers (Byte,Addr,Value)
import Text.Printf (printf)

-- TODO: code unlink so can reinstate checking
checking :: Bool
checking = True -- enable the well-formedness and tree-size checks (and pay for it -- see stats!)

dump :: Eff ()
dump = do
  let os = [1..248]
  zv <- getZversion
  base <- objectTableBase
  objects <- mapM (getObject base zv) os
  mapM_ (GamePrint . printf "%s\n" . show) objects

getShortName :: Int -> Eff String
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
  --Debug ("testAttr",o,n)
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
  --Debug ("getProp",o,n)
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{propTable=PropTable{props}} = ob
  let xs = [ dataBytes | Prop{number,dataBytes} <- props, number == n ]
  x <-
    case xs of
      [x] -> pure x
      [] -> do
        let defaultValue = 0 -- TODO
        --Debug ("getProp(USE DEFAULT)",o,n,defaultValue)
        pure [defaultValue]
      _ ->
        error "multi prop match"
  case x of
    [hi,lo] -> pure (256 * fromIntegral hi + fromIntegral lo)
    [b] -> pure $ fromIntegral b
    _ -> error "expected 1 or 2 bytes for prop value"


putProp :: Int -> Int -> Value -> Eff ()
putProp o n v = do
  --Debug ("puttProp",o,n,v)
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  --Debug(ob)
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
      setByte (fromIntegral a) hi
      setByte (fromIntegral (a+1)) lo
      pure ()
    1 -> undefined
    _ -> error "expected 1 or 2 bytes for prop value"
  --ob <- getObject base zv o
  --Debug(ob)

setByte :: Addr -> Byte -> Eff ()
setByte a b = do
  --Debug ("SetByte",a,b)
  SetByte a b


getPropAddr :: Int -> Int -> Eff Value
getPropAddr o n = do
  --Debug ("getPropAddr",o,n)
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
  --Debug ("getPropLen",a)
  if a == 0 then pure 0 else do
    --Debug("getPropLen,a=",a)
    b <- GetByte (fromIntegral a - 1) -- TODO: ** bug here ** -- need "-1"
    --Debug("getPropLen,b=",b)
    let numBytes :: Int = 1 + fromIntegral ((b `shiftR` 5) .&. 0x7) -- copied from getProps -- TODO: Is this +1 correct???
    let res = fromIntegral numBytes
    --Debug("getPropLen,res=",res)
    pure res

unlink :: Int -> Eff ()
unlink this = do
  --Debug("unlink, this=",this)
  oldP <- getParentQ this
  --Debug("old-parent",oldP)
  when (oldP /= 0) $ do
    --Debug ("old-parent not zero, so must unlink")
    --seeObjRels oldP
    child <- getChildQ oldP
    --Debug ("first child is:",child)
    case child == this of
      True -> do
        --Debug "special case for unlink first child"
        thisSib <- getSibling this
        --Debug ("relinking oldP's child to:", thisSib)
        setChild oldP (byteOfInt thisSib)
        --seeObjRels oldP
      False -> do
        --Debug ("not the first child, so beginning unlink loop")
        loop child
      where
        loop :: Int -> Eff ()
        loop x = do
          --Debug ("unlink loop, child=",x)
          when (x == 0) $ error "unlink loop, failed to find unlinkee"
          sib <- getSibling x
          --Debug ("sib=",sib)
          case  sib == this of
            False -> loop sib
            True -> do
              --Debug ("unlink loop, found it!, relinking s's sib to:")
              thisSib <- getSibling this
              --Debug ("relinking x's sib to:", thisSib)
              setSibling x (byteOfInt thisSib)


insertObj :: Int -> Int -> Eff ()
insertObj o dest = do
  --Debug ("insertObj",o,dest)
  --seeObjRels o
  --seeObjRels dest
  assertWellFormed
  sizeBefore <- sizeObjectTree
  --Debug (sizeBefore)
  --_oldP <- getParentQ o
  --Debug("old-parent",_oldP)
  --when (_oldP /= 0) $ Debug "TODO: need to unlink non-zero-parent"
  --when (_oldP == 0) $ do
  unlink o
  assertWellFormed
  --seeObjRels o
  --seeObjRels dest
  --Debug ("unlink has preserve well formedness, so now insert obj into new pos")
  do
    setParent o (byteOfInt dest)
    oldChild <- getChildQ dest
    setSibling o (byteOfInt oldChild)
    setChild dest (byteOfInt o)
    assertWellFormed
    sizeAfter <- sizeObjectTree
    --Debug (sizeAfter)
    when (sizeAfter /= sizeBefore) $ error (show ("size of object tree has changed",sizeBefore,sizeAfter))
    --seeObjRels o
    --seeObjRels dest


byteOfInt :: Int -> Byte
byteOfInt i = do
  if i < 0 || i > 255 then error (show ("byteOfInt",i)) else fromIntegral i

sizeObjectTree :: Eff Int
sizeObjectTree = if not checking then pure 0 else do
  roots <- objectRoots
  forest <- mapM getTree roots
  pure $ sizeForest forest

objectRoots :: Eff [Int]
objectRoots = do
  let os = [1::Int ..248]
  ops <- sequence [ do p <- getParentQ o; pure (o,p) | o <- os ]
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

setParent :: Int -> Byte -> Eff ()
setParent x p = do
  --Debug ("setParent",x,p)
  zv <- getZversion
  base <- objectTableBase
  a <- objectAddr base zv x
  SetByte (a+4) p

setSibling :: Int -> Byte -> Eff ()
setSibling x p = do
  --Debug ("setSibling",x,p)
  zv <- getZversion
  base <- objectTableBase
  a <- objectAddr base zv x
  SetByte (a+5) p

setChild :: Int -> Byte -> Eff () -- TODO: dont pass base/z
setChild x p = do
  --Debug ("setChild",x,p)
  zv <- getZversion
  base <- objectTableBase
  a <- objectAddr base zv x
  SetByte (a+6) p

assertWellFormed :: Eff ()
assertWellFormed = if not checking then pure () else do
  wf <- wellFormed
  if wf then pure () else error "not well formed"

wellFormed :: Eff Bool -- TODO: flesh out well-formed to detect more problem cases
wellFormed = do
  let os = [1::Int ..248]
  xs <- sequence [ do
                     cs <- children o
                     ps <- sequence [getParentQ c | c <- cs]
                     let ws = [ p == o | p <- ps ]
                     pure (o,cs,ps,ws,all Prelude.id ws)
                 | o <- os ]
  --mapM_ Debug xs
  let wf = all Prelude.id [ b | (_,_,_,_,b) <- xs ]
  pure wf

children :: Int -> Eff [Int]
children p = do
  getChildQ p >>= sibList

sibList :: Int -> Eff [Int]
sibList x = do
  if x == 0 then pure [] else do
    y <- getSiblingQ x
    ys <- sibList y
    pure (x:ys)


_seeObjRels :: Int -> Eff ()
_seeObjRels ob = do
  p <- getParentQ ob
  s <- getSiblingQ ob
  c <- getChildQ ob
  Debug ("**SEE OB RELS: ob=",ob, "parent=",p,"sib=",s,"child=",c)

getParent :: Int -> Eff Int
getParent o = do
  res <- getParentQ o
  --Debug ("getParent",o,"->",res)
  pure res

getSibling :: Int -> Eff Int
getSibling o = do
  res <- getSiblingQ o
  --Debug ("getSibling",o,"->",res)
  pure res

getChild :: Int -> Eff Int
getChild o = do
  res <- getChildQ o
  --Debug ("getChild",o,"->",res)
  pure res

getParentQ :: Int -> Eff Int -- TODO: capture common pattern for parent/sibling/child
getParentQ o = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{parent} = ob
  pure parent

getSiblingQ :: Int -> Eff Int
getSiblingQ o = do
  zv <- getZversion
  base <- objectTableBase
  ob <- getObject base zv o
  let Object{sibling} = ob
  pure sibling

getChildQ :: Int -> Eff Int
getChildQ o = do
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

data Object = Object
  { id :: Int
  , atts :: Attributes
  , parent :: Int
  , sibling :: Int
  , child :: Int
  , propTable :: PropTable
  }
--  deriving Show

instance Show Object where
  show  Object{id,propTable=PropTable{shortName}}  =
    printf "%d: %s" id shortName

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

getObject :: Addr -> Zversion -> Int -> Eff Object -- TODO: avoid calling this so often
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
