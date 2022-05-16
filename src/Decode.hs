
module Decode
  ( fetchInstruction
  , fetchRoutineHeader
  , makeTarget
  , ztext
  ) where

import Data.Array ((!),listArray)
import Data.Bits (testBit,(.&.),(.|.),shiftL,shiftR)
import Fetch (Fetch(..))
import Instruction (Instruction,Func(..),Args(..),Arg(..),Target(..),Label(..),Dest(..),Sense(T,F),RoutineHeader(..))
import Numbers (Byte,Addr,Value,addrOfPackedWord)
import Text.Printf (printf)
import qualified Data.Char as Char
import qualified Instruction as I

data Op = Op Byte [RandType]
  deriving Show

data RandType = ByteConst | WordConst | ByteVariable
  deriving Show

fetchInstruction :: Fetch Instruction
fetchInstruction = fetchOp >>= \case
  Op 1 ts -> I.Je <$> args ts <*> label
  Op 193 ts -> I.Je <$> args ts <*> label
  Op 2 [t1,t2] -> I.Jl <$> arg t1 <*> arg t2 <*> label
  Op 3 [t1,t2] -> I.Jg <$> arg t1 <*> arg t2 <*> label
  Op 195 [t1,t2] -> I.Jg <$> arg t1 <*> arg t2 <*> label
  Op 4 [t1,t2] -> I.Dec_check <$> arg t1 <*> arg t2 <*> label
  Op 5 [t1,t2] -> I.Inc_check <$> arg t1 <*> arg t2 <*> label
  Op 6 [t1,t2] -> I.Jin <$> arg t1 <*> arg t2 <*> label
  Op 7 [t1,t2] -> I.Test <$> arg t1 <*> arg t2 <*> label
  Op 9 [t1,t2] -> I.And_ <$> arg t1 <*> arg t2 <*> target
  Op 201 [t1,t2] -> I.And_ <$> arg t1 <*> arg t2 <*> target
  Op 10 [t1,t2] -> I.Test_attr <$> arg t1 <*> arg t2 <*> label
  Op 11 [t1,t2] -> I.Set_attr <$> arg t1 <*> arg t2
  Op 12 [t1,t2] -> I.Clear_attr <$> arg t1 <*> arg t2
  Op 13 [t1,t2] -> I.Store <$> arg t1 <*> arg t2
  Op 205 [t1,t2] -> I.Store <$> arg t1 <*> arg t2
  Op 14 [t1,t2] -> I.Insert_obj <$> arg t1 <*> arg t2
  Op 15 [t1,t2] -> I.Load_word <$> arg t1 <*> arg t2 <*> target
  Op 16 [t1,t2] -> I.Load_byte <$> arg t1 <*> arg t2 <*> target
  Op 17 [t1,t2] -> I.Get_prop <$> arg t1 <*> arg t2 <*> target
  Op 18 [t1,t2] -> I.Get_prop_addr <$> arg t1 <*> arg t2 <*> target
  Op 19 [t1,t2] -> I.Get_next_prop <$> arg t1 <*> arg t2 <*> target
  Op 20 [t1,t2] -> I.Add <$> arg t1 <*> arg t2 <*> target
  Op 21 [t1,t2] -> I.Sub <$> arg t1 <*> arg t2 <*> target
  Op 22 [t1,t2] -> I.Mul <$> arg t1 <*> arg t2 <*> target
  Op 23 [t1,t2] -> I.Div <$> arg t1 <*> arg t2 <*> target
  Op 128 [t] -> I.Jz <$> arg t <*> label
  Op 129 [t] -> I.Get_sibling <$> arg t <*> target <*> label
  Op 130 [t] -> I.Get_child <$> arg t <*> target <*> label
  Op 131 [t] -> I.Get_parent <$> arg t <*> target
  Op 132 [t] -> I.Get_prop_len <$> arg t <*> target
  Op 133 [t] -> I.Inc <$> arg t
  Op 134 [t] -> I.Dec <$> arg t
  Op 135 [t] -> I.Print_addr <$> arg t
  Op 138 [t] -> I.Print_obj <$> arg t
  Op 139 [t] -> I.Return <$> arg t
  Op 140 [WordConst] -> I.Jump <$> jumpLocation
  Op 141 [t] -> I.Print_paddr <$> arg t
  Op 176 [] -> pure I.Rtrue
  Op 177 [] -> pure I.Rfalse
  Op 184 [] -> pure I.Ret_popped
  Op 178 [] -> I.Print <$> ztext
  Op 179 [] -> I.Print_ret <$> ztext
  Op 187 [] -> pure I.New_line
  Op 197 [t1,t2] -> I.Inc_check <$> arg t1 <*> arg t2 <*> label
  Op 224 (t1:ts) -> I.Call <$> func t1 <*> args ts <*> target
  Op 225 [t1,t2,t3] -> I.Storew <$> arg t1 <*> arg t2 <*> arg t3
  Op 226 [t1,t2,t3] -> I.Storeb <$> arg t1 <*> arg t2 <*> arg t3
  Op 228 [t1,t2] -> I.Sread <$> arg t1 <*> arg t2
  Op 227 [t1,t2,t3] -> I.Put_prop <$> arg t1 <*> arg t2 <*> arg t3
  Op 229 [t] -> I.Print_char <$> arg t
  Op 230 [t] -> I.Print_num <$> arg t
  Op 231 [t] -> I.Random <$> arg t <*> target
  Op 232 [t] -> I.Push <$> arg t
  Op 233 [t] -> I.Pull <$> arg t
  op -> error (printf "unknown instruction: %s" (show op))

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
        Just ty -> pure (Op (x .&. 0xCF) [ty])
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
  ByteConst -> error "func, ByteConst"
  WordConst -> (Floc . addrOfPackedWord) <$> fetchNextWord
  ByteVariable -> (Fvar . makeTarget) <$> Fetch.NextByte

args :: [RandType] -> Fetch Args
args ts = Args <$> mapM arg ts

target :: Fetch Target
target = makeTarget <$> Fetch.NextByte

arg :: RandType -> Fetch Arg
arg = \case
  ByteConst -> (Con . fromIntegral) <$> Fetch.NextByte
  WordConst -> Con <$> fetchNextWord
  ByteVariable -> (Var . makeTarget) <$> Fetch.NextByte

makeTarget :: Byte -> Target
makeTarget = \case
  0 -> Sp
  n -> if n < 16 then Local n else Global (n - 16)

label :: Fetch Label
label = do
  x <- Fetch.NextByte
  let sense = if x `testBit` 7 then T else F
  let small = x `testBit` 6
  case small of
    True -> do
      -- interpret 6bit number as unsigned; small branches are always forward
      let offset = fromIntegral (x .&. 0x3F)
      dest <- decodeDest offset
      pure (Branch sense dest)
    False -> do
      y <- Fetch.NextByte
      let offset :: Int = fromIntegral (x .&. 0x3F) `shiftL` 8 .|. fromIntegral y
      let signed = x `testBit` 5
      -- interpret 14bit number as signed; branches can be forwards or backwards
      dest <- decodeDest (if signed then offset - 0x4000 else offset)
      pure (Branch sense dest)

decodeDest :: Int -> Fetch Dest
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
  pure (here + fromIntegral w)

fetchNextWord :: Fetch Value
fetchNextWord = do
  hi <- Fetch.NextByte
  lo <- Fetch.NextByte
  pure (256 * fromIntegral hi + fromIntegral lo)


fetchRoutineHeader :: Fetch RoutineHeader
fetchRoutineHeader = do
  n <- Fetch.NextByte
  if n > 15 then error "fetchRoutineHeader, n>15" else do
    ws <- sequence (take (fromIntegral n) (repeat fetchNextWord))
    pure (RoutineHeader ws)

----------------------------------------------------------------------

getWord :: Addr -> Fetch Value
getWord a = do
  hi <- getByte a
  lo <- getByte (a+1)
  -- TODO: share various instances of lo/hi composition
  pure (256 * fromIntegral hi + fromIntegral lo)

getByte :: Addr -> Fetch Byte
--getByte = GetByte
getByte a = WithPC a NextByte

ztext :: Fetch String
ztext = loop [] A0 A0 []
  where
    loop :: [Value] -> Alpha -> Alpha -> [Char] -> Fetch String
    loop delayedZs alpha lock acc = do
      x <- fetchNextWord
      let stop = (x `testBit` 15)
      let z1 = (x `shiftR` 10) .&. 0x1f
      let z2 = (x `shiftR` 5) .&. 0x1f
      let z3 = x .&. 0x1f
      inner alpha lock stop acc (delayedZs ++ [z1,z2,z3])

    inner :: Alpha -> Alpha -> Bool -> [Char] -> [Value] -> Fetch String
    inner alpha lock stop acc = \case
      []
        | stop -> pure (reverse acc)
        | otherwise -> loop [] alpha lock acc

      0:zs ->
        inner alpha lock stop (' ' : acc) zs

      [z] | z `elem` [1,2,3] -> loop [z] alpha lock acc

      n:z:zs | n `elem` [1,2,3] -> do
        expansion <- decodeAbbrev (z + (n-1) * 32)
        inner lock lock stop (reverse expansion ++ acc) zs

      4:zs -> inner (shiftUp alpha) lock stop acc zs
      5:zs -> inner (shiftDown alpha) lock stop acc zs

      6:a:b:zs | (alpha == A2) -> do
        let c = Char.chr (fromIntegral a * 32 + fromIntegral b)
        inner lock lock stop (c : acc) (zs)

      zs@(6:_) | (alpha == A2) -> loop zs alpha lock acc

      z:zs -> inner lock lock stop (decodeAlphabet alpha (z - 6) : acc) zs


decodeAbbrev :: Value -> Fetch String
decodeAbbrev n = do
  baseAbbrev :: Addr <- fromIntegral <$> getWord 0x18 -- TODO: get from header
  thisAbbrev :: Addr <- addrOfPackedWord <$> getWord (baseAbbrev + fromIntegral (2 * n))
  WithPC thisAbbrev ztext


data Alpha = A0 | A1 | A2 deriving Eq

shiftUp :: Alpha -> Alpha
shiftUp = \case A0 -> A1; A1 -> A2; A2 -> A0

shiftDown :: Alpha -> Alpha
shiftDown = \case A0 -> A2; A1 -> A0; A2 -> A1

decodeAlphabet :: Alpha -> Value -> Char
decodeAlphabet alpha i = if
  | (i < 0 || i > 25) -> error (show ("decodeAlphabet",i))
  | otherwise -> a ! i
  where
    a = case alpha of A0 -> a0; A1 -> a1; A2 -> a2
    a0 = listArray (0,25) "abcdefghijklmnopqrstuvwxyz"
    a1 = listArray (0,25) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    a2 = listArray (0,25) " \n0123456789.,!?_#'\"/\\-:()"
    --a2':: Array Word Char = listArray (0,25) " 0123456789.,!?_#'\"/\\<-:()" -- Z1
