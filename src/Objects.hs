
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
import Numbers (Addr,Value)

type Effect b x = Eff Addr b String Value x -- TODO: generalise further

--[convs]-----------------------------------------------------

v2a :: Value -> Addr
v2a = fromIntegral

getWord :: Addr -> Effect b Value
getWord a = do
  hi <- GetByte a
  lo <- GetByte (a+1)
  MakeWord hi lo

objectAddr :: Value -> Effect b Addr -- TODO: move static calcs to header?
objectAddr o = do
  Header{zv,objectTable} <- StoryHeader
  let base = objectTable
  let f = formatOfVersion zv
  let objectEntrySize = numByesForAttribute f + (3 * objectIdSize f) + 2
  let propDefaultsSize = 2 * numProps f
  pure (base + v2a (propDefaultsSize + (o-1) * objectEntrySize))

getShortName :: Value -> Effect b String
getShortName x = do
  a <- objectAddr x
  a' <- getWord (a+7)
  shortNameLen <- GetByte (v2a a')
  p <- IsZeroByte shortNameLen
  if p then pure "" else GetText (v2a (a'+1))

--[attributes]--------------------------------------------------------

testAttr :: Value -> Value -> Effect b Bool
testAttr x n = do
  a <- objectAddr x
  d <- Div8 n
  m <- Mod8 n
  m' <- SevenMinus m
  let aa = a + v2a d
  b <- GetByte aa
  b `TestBit` m'

setAttr :: Value -> Value -> Effect b ()
setAttr x n = do
  a <- objectAddr x
  d <- Div8 n
  m <- Mod8 n
  let aa = a + v2a d
  old <- GetByte aa
  m' <- SevenMinus m
  new <- old `SetBit` m'
  SetByte aa new

clearAttr :: Value -> Value -> Effect b ()
clearAttr x n = do
  a <- objectAddr x
  d <- Div8 n
  m <- Mod8 n
  let aa = a + v2a d
  old <- GetByte aa
  m' <- SevenMinus m
  new <- old `ClearBit` m'
  SetByte aa new

--[object containment hierarchy]--------------------------------------

data FamilyMember = Parent | Sibling | Child

offsetFM :: FamilyMember -> Value
offsetFM = \case
  Parent -> 4
  Sibling -> 5
  Child -> 6

getFM :: FamilyMember -> Value -> Effect b Value
getFM fm x = do
  a <- objectAddr x
  b <- GetByte (a + v2a (offsetFM fm))
  Widen b

setFM :: FamilyMember -> Value -> Value -> Effect b ()
setFM fm x y = do
  a <- objectAddr x
  lo <- LoByte y
  SetByte (a + v2a (offsetFM fm)) lo

insertObj :: Value -> Value -> Effect b ()
insertObj x dest = do
  unlink x
  setFM Parent x dest
  oldChild <- getFM Child dest
  setFM Sibling x oldChild
  setFM Child dest x

removeObj :: Value -> Effect b ()
removeObj x = do
  unlink x
  setFM Parent x 0

unlink :: Value -> Effect b ()
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
        loop :: Value -> Effect b ()
        loop x = do
          when (x == 0) $ error "unlink loop, failed to find unlinkee"
          sib <- getFM Sibling x
          case  sib == this of
            False -> loop sib
            True -> do
              thisSib <- getFM Sibling this
              setFM Sibling x thisSib

--[properties]--------------------------------------------------------

getProp :: Show b => Value -> Value -> Effect b Value
getProp x n = do
  Header{objectTable=base} <- StoryHeader
  props <- getPropertyTable x
  case [ dataBytes | Prop{number,dataBytes} <- props, number == n ] of
    [] -> do
      -- get property default
      getWord (base + v2a (2 * (n-1)))
    [prop] -> do
      case prop of
        [hi,lo] -> MakeWord hi lo
        [b] -> undefined (Widen b) -- not hit yet
        _ -> error "expected 1 or 2 bytes for prop value"
    ps ->
      error (show ("getProp: multi prop match",ps))

putProp :: Show b => Value -> Value -> Value -> Effect b ()
putProp x n v = do
  props <- getPropertyTable x
  let xs = [ pr | pr@Prop{number} <- props, number == n ]
  pr <-
    case xs of
      [x] -> pure x
      [] -> error "putProp, no such prop"
      ps -> error (show ("putProp: multi prop match",ps))

  let Prop{dataBytes,dataAddr=a} = pr
  case length dataBytes  of
    2 -> do
      hi <- HiByte v
      lo <- LoByte v
      SetByte (v2a a) hi
      SetByte (v2a (a+1)) lo
      pure ()
    1 -> undefined
    _ -> error "expected 1 or 2 bytes for prop value"

getPropAddr :: Value -> Value -> Effect b Value
getPropAddr x n = do
  props <- getPropertyTable x
  case [ dataAddr | Prop{number,dataAddr} <- props, number == n ] of
    [a] -> pure a
    [] -> pure 0
    ps -> error (show ("getPropAddr: multi prop match",ps))

getPropLen :: Value -> Effect b Value
getPropLen a = do
  if a == 0 then pure 0 else do
    b <- GetByte (v2a (a - 1))
    shifted <- b `ShiftR` 5
    seven <- LitB 0x7
    masked <- shifted `BwAnd` seven
    v <- Widen masked
    one <- LitV 1
    BinOp BAdd one v

getNextProp :: Value -> Value -> Effect b Value
getNextProp x p = do
  props <- getPropertyTable x
  let bigger = [ n | Prop{number=n} <- props, p == 0 || n < p ]
  case bigger of
    [] -> pure 0
    _ -> pure $ maximum bigger

getPropertyTable :: Value -> Effect b [Prop b]
getPropertyTable x = do
  a <- objectAddr x
  a' <- getWord (a+7)
  shortNameLen <- GetByte (v2a a')
  offset <- dubPlus1 shortNameLen
  props <- getPropsA (a' + offset)
  pure (takeWhileDescending props)

dubPlus1 :: b -> Effect b Value
dubPlus1 b = do --Widen (1 + 2 * shortNameLen)
  v <- Widen b
  one <- LitV 1
  two <- LitV 2
  dub <- BinOp BMul two v
  BinOp BAdd dub one

takeWhileDescending :: [Prop b] -> [Prop b]
takeWhileDescending = \case
  [] -> []
  p@Prop{number}:ps -> p : loop number ps
    where
      loop last = \case
        [] -> []
        p@Prop{number}:ps -> if
          | number < last -> p : loop number ps
          | otherwise -> []

data Prop b = Prop { number :: Value, dataAddr :: Value, dataBytes :: [b] } deriving Show

getPropsA :: Value -> Effect b [Prop b]
getPropsA a = do
  b <- GetByte (v2a a)
  p <- IsZeroByte b
  if p then pure [] else do
    oneF <- LitB 0x1f
    fiveBits <- b `BwAnd` oneF
    number <- Widen fiveBits
    shifted <- b `ShiftR` 5
    widened <- Widen shifted
    one <- LitV 1
    numBytes <- BinOp BAdd widened one
    let dataAddr = a + 1
    dataBytes <- getBytes (a+1) numBytes
    let p1 = Prop {number,dataAddr,dataBytes}
    more <- getPropsA (a + numBytes + 1) -- TODO: loop
    pure (p1:more)

getBytes :: Value -> Value -> Effect b [b]
getBytes a n =
  sequence [do a' <- BinOp BAdd a i; GetByte (v2a a')
           | i <- [0.. n - 1]
           ]

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

