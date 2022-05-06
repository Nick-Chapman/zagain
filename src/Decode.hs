
module Decode (fetchInstruction) where

import Addr(Addr)
import Data.Bits (testBit,(.&.))
import Data.Word (Word8)
import Fetch (Fetch)
import Instruction (Instruction,Func(..),Args(..),Arg(..),Variable(..),Label(..),Dest(..),Boolean(T,F))
import Text.Printf (printf)
import qualified Addr
import qualified Fetch
import qualified Instruction as I

type Byte = Word8

data Op = Op Byte [RandType]
  deriving Show

data RandType = ByteConst | WordConst | ByteVariable
  deriving Show

fetchInstruction :: Fetch Instruction
fetchInstruction = fetchOp >>= \case
  Op 10 [t1,t2] -> I.Test_attr <$> arg t1 <*> arg t2 <*> label
  Op 13 [t1,t2] -> I.Store <$> arg t1 <*> arg t2
  Op 14 [t1,t2] -> I.Insert_obj <$> arg t1 <*> arg t2
  Op 20 [t1,t2] -> I.Add <$> arg t1 <*> arg t2 <*> target
  Op 140 [WordConst] -> I.Jump <$> jumpLocation
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
        Just ty -> pure (Op x [ty])
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
