
module Decode
  ( fetchInstruction
  , fetchRoutineHeader
  ) where

import Addr (Addr)
import Data.Array (Array,(!),listArray)
import Data.Bits (testBit,(.&.),shiftR)
import Data.Word (Word8)
import Fetch (Fetch(..))
import Instruction (Instruction,Func(..),Args(..),Arg(..),Variable(..),Label(..),Dest(..),Boolean(T,F),RoutineHeader(..))
import Text.Printf (printf)
import qualified Addr
import qualified Instruction as I

type Byte = Word8

data Op = Op Byte [RandType]
  deriving Show

data RandType = ByteConst | WordConst | ByteVariable
  deriving Show

fetchInstruction :: Fetch Instruction
fetchInstruction = fetchOp >>= \case
  Op 1 ts -> I.Je <$> args ts <*> label
  Op 10 [t1,t2] -> I.Test_attr <$> arg t1 <*> arg t2 <*> label
  Op 13 [t1,t2] -> I.Store <$> arg t1 <*> arg t2
  Op 14 [t1,t2] -> I.Insert_obj <$> arg t1 <*> arg t2
  Op 20 [t1,t2] -> I.Add <$> arg t1 <*> arg t2 <*> target
  Op 138 [t] -> I.Print_obj <$> arg t
  Op 140 [WordConst] -> I.Jump <$> jumpLocation
  Op 178 [] -> I.Print  <$> ztext
  Op 179 [] -> I.Print_ret  <$> ztext
  Op 187 [] -> pure I.New_line
  Op 224 (t1:ts) -> I.Call <$> func t1 <*> args ts <*> target
  Op 225 [t1,t2,t3] -> I.Storew <$> arg t1 <*> arg t2 <*> arg t3
  Op 227 [t1,t2,t3] -> I.Put_prop <$> arg t1 <*> arg t2 <*> arg t3
  op ->
    Fetch.Err (printf "illegal: %s" (show op))

data Form = LongForm | ShortForm | VarForm

fetchOp :: Fetch Op
fetchOp = do
  x <- Fetch.NextByte
  case decodeForm x of
    LongForm -> do
      let t1 = decodeLongRandType x 6
      let t2 = decodeLongRandType x 5
      pure (Op (x .&. 0x1F) [t1,t2])
    ShortForm -> do
      case decodeRandType x (5,4) of
        Nothing -> pure (Op x [])
        Just ty -> pure (Op (x .&. 0xDF) [ty])
    VarForm -> do
      x2 <- Fetch.NextByte
      let tys = decodeRandTypes x2
      pure (Op x tys)

decodeForm :: Byte -> Form
decodeForm b = case b `testBit` 7 of
  False -> LongForm
  True -> case b `testBit` 6 of
    False -> ShortForm
    True -> VarForm

decodeRandTypes :: Byte -> [RandType]
decodeRandTypes x =
  case decodeRandType x (7,6) of
    Nothing -> []
    Just ty1 -> case decodeRandType x (5,4) of
      Nothing -> [ty1]
      Just ty2 -> case decodeRandType x (3,2) of
        Nothing -> [ty1,ty2]
        Just ty3 -> case decodeRandType x (1,0) of
          Nothing -> [ty1,ty2,ty3]
          Just ty4 -> [ty1,ty2,ty3,ty4]

decodeRandType :: Byte -> (Int,Int) -> Maybe RandType
decodeRandType x (a,b) =
  case (x `testBit` a, x `testBit` b) of
    (False,False) -> Just WordConst
    (False,True) -> Just ByteConst
    (True,False) -> Just ByteVariable
    (True,True) -> Nothing

decodeLongRandType :: Byte -> Int -> RandType
decodeLongRandType x a =
  case x `testBit` a of
    True -> ByteVariable
    False -> ByteConst

func :: RandType -> Fetch Func
func = \case
  ByteConst{} -> Fetch.Err "func, ByteConst"
  WordConst{} -> (Floc . Addr.ofPackedWord) <$> fetchNextWord
  ByteVariable{} -> Fetch.Err "ByteVariable"

args :: [RandType] -> Fetch Args
args ts = Args <$> mapM arg ts

target :: Fetch Variable
target = makeVariable <$> Fetch.NextByte

arg :: RandType -> Fetch Arg
arg = \case
  ByteConst -> (Con . fromIntegral) <$> Fetch.NextByte
  WordConst -> (Con . fromIntegral) <$> fetchNextWord
  ByteVariable -> (Var . makeVariable) <$> Fetch.NextByte

makeVariable :: Byte -> Variable
makeVariable = \case
  0 -> Sp
  n -> if n < 16 then Local n else Global (n - 16)

label :: Fetch Label
label = do
  x <- Fetch.NextByte
  let sense = if x `testBit` 7 then T else F
  let small = x `testBit` 6
  case small of
    True -> do
      -- interpret 6bit number as unsigned; small braches are always forward!
      let offset = x .&. 0x3F
      dest <- decodeDest offset
      pure (Branch sense dest)
    False ->
      Fetch.Err "big label"

decodeDest :: Byte -> Fetch Dest
decodeDest = \case
  0 -> pure Dfalse
  1 -> pure Dtrue
  offset -> do
    here <- Fetch.Here
    pure $ Dloc (here + fromIntegral offset - 2)

jumpLocation :: Fetch Addr
jumpLocation = do
  here <- Fetch.Here
  w <- fetchNextWord
  pure $ fromIntegral (fromIntegral here + decodeSigned w)

decodeSigned :: Word -> Int
decodeSigned w =
  if w `testBit` 15
  then fromIntegral w - 0x10000
  else fromIntegral w

fetchNextWord :: Fetch Word
fetchNextWord = do
  hi <- Fetch.NextByte
  lo <- Fetch.NextByte
  pure (256 * fromIntegral hi + fromIntegral lo)


fetchRoutineHeader :: Fetch RoutineHeader
fetchRoutineHeader = do
  n <- Fetch.NextByte
  if n > 15 then Fetch.Err "fetchRoutineHeader, n>15" else do
    ws <- sequence (take (fromIntegral n) (repeat fetchNextWord))
    pure (RoutineHeader (map fromIntegral ws))

----------------------------------------------------------------------
data Alpha = A0 | A1 | A2

shiftUp :: Alpha -> Alpha
shiftUp = \case A0 -> A1; A1 -> A2; A2 -> A0

shiftDown :: Alpha -> Alpha
shiftDown = \case A0 -> A2; A1 -> A0; A2 -> A1

deco :: Alpha -> Word -> Char
deco alpha i = if i <0 || i > 25 then error (show ("deco",i)) else a ! i
  where
    a = case alpha of A0 -> a0; A1 -> a1; A2 -> a2
    a0 :: Array Word Char = listArray (0,25) "abcdefghijklmnopqrstuvwxyz"
    a1 :: Array Word Char = listArray (0,25) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    a2 :: Array Word Char = listArray (0,25) " \n0123456789.,!?_#'\"/\\-:()"
    --a2':: Array Word Char = listArray (0,25) " 0123456789.,!?_#'\"/\\<-:()" -- Z1

abbrev :: Word -> Fetch String
abbrev n = do
  baseAbbrev :: Addr <- fromIntegral <$> getWord 0x18 -- should we avoid the repeated fetch?
  thisAbbrev :: Addr <- Addr.ofPackedWord <$> getWord (baseAbbrev + fromIntegral (2 * n))
  WithPC thisAbbrev ztext

getWord :: Addr -> Fetch Word
getWord a = do
  hi <- GetByte a
  lo <- GetByte (a+1)
  pure (256 * fromIntegral hi + fromIntegral lo)

ztext :: Fetch String
ztext = loop [] A0 A0 []
  where
    loop :: [Word] -> Alpha -> Alpha -> [Char] -> Fetch String
    loop delayedZs alpha lock acc = do
      x <- fetchNextWord
      let stop = (x `testBit` 15)
      let z1 = (x `shiftR` 10) .&. 0x1f
      let z2 = (x `shiftR` 5) .&. 0x1f
      let z3 = x .&. 0x1f
      inner alpha lock stop acc (delayedZs ++ [z1,z2,z3])

    inner :: Alpha -> Alpha -> Bool -> [Char] -> [Word] -> Fetch String
    inner alpha lock stop acc = \case
      [] ->
        if stop then pure (reverse acc) else loop [] alpha lock acc
      0:zs ->
        inner alpha lock stop (' ' : acc) zs
      [z@1] -> do
        loop [z] alpha lock acc
      1:z:zs -> do
        expansion <- abbrev z
        inner alpha lock stop (reverse expansion ++ acc) zs
      [z@2] -> do
        loop [z] alpha lock acc
      2:z:zs -> do
        expansion <- abbrev (z + 32)
        inner alpha lock stop (reverse expansion ++ acc) zs
      [3] ->
        undefined
      3:zs ->
        undefined zs
      4:zs ->
        inner (shiftUp alpha) lock stop acc zs
      5:zs ->
        inner (shiftDown alpha) lock stop acc zs
      z:zs ->
        -- revert to pre-lock
        inner lock lock stop (deco alpha (z - 6) : acc) zs
