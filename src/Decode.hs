
module Decode (decodeInstruction) where

import Addr(Addr)
import Control.Monad (ap,liftM)
import Data.Bits (testBit,(.&.))
import Data.Word (Word8)
import Instruction (Instruction,Func(..),Args(..),Arg(..),Variable(..),Label(..),Dest(..),Boolean(T,F))
import qualified Addr
import qualified Instruction as I

type Byte = Word8

decodeInstruction :: Addr -> [Byte] -> (Instruction,Int)
decodeInstruction = runFetch fetchInstruction

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
    error (show ("fetchInstructionForOp",op))

data Form = LongForm | ShortForm | VarForm

fetchOp :: Fetch Op
fetchOp = do
  x <- FetchByte
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
      x2 <- FetchByte
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
  ByteConst{} -> error "func, ByteConst"
  WordConst{} -> (Floc . Addr.ofPackedWord) <$> fetchWord
  ByteVariable{} -> undefined

args :: [RandType] -> Fetch Args
args ts = Args <$> mapM arg ts

target :: Fetch Variable
target = makeVariable <$> FetchByte

arg :: RandType -> Fetch Arg
arg = \case
  ByteConst -> (Con . fromIntegral) <$> FetchByte
  WordConst -> (Con . fromIntegral) <$> fetchWord
  ByteVariable -> (Var . makeVariable) <$> FetchByte

makeVariable :: Byte -> Variable
makeVariable = \case
  0 -> Sp
  n -> if n < 16 then Local n else Global (n - 16)

label :: Fetch Label
label = do
  x <- FetchByte
  let sense = if x `testBit` 7 then T else F
  let small = x `testBit` 6
  case small of
    True -> do
      -- interpret 6bit number as unsigned; small braches are always forward!
      let offset = x .&. 0x3F
      dest <- decodeDest offset
      pure (Branch sense dest)
    False ->
      undefined

decodeDest :: Byte -> Fetch Dest
decodeDest = \case
  0 -> pure Dfalse
  1 -> pure Dtrue
  offset -> do
    here <- Here
    pure $ Dloc (here + fromIntegral offset - 2)

jumpLocation :: Fetch Addr
jumpLocation = do
  here <- Here
  w <- fetchWord
  pure $ fromIntegral (fromIntegral here + decodeSigned w)

decodeSigned :: Word -> Int
decodeSigned w =
  if w `testBit` 15
  then fromIntegral w - 0x10000
  else fromIntegral w

fetchWord :: Fetch Word
fetchWord = do
  hi <- FetchByte
  lo <- FetchByte
  pure (256 * fromIntegral hi + fromIntegral lo)


instance Functor Fetch where fmap = liftM
instance Applicative Fetch where pure = return; (<*>) = ap
instance Monad Fetch where return = Ret; (>>=) = Bind

data Fetch a where
  Ret :: a -> Fetch a
  Bind :: Fetch a -> (a -> Fetch b) -> Fetch b
  FetchByte :: Fetch Byte
  Here :: Fetch Addr

runFetch :: Fetch a -> Addr -> [Byte]-> (a,Int)
runFetch m pc bs = (res,resN)
  where
    (res,_,resN) = loop bs 0 m
    loop :: [Byte] -> Int -> Fetch a -> (a,[Byte],Int)
    loop bs n m = case m of
      Ret a -> (a,bs,n)
      Bind m f -> let (a,bs',n') = loop bs n m in loop bs' n' (f a)
      FetchByte -> case bs of
        [] -> error "runFetch"
        b:bs -> (b,bs,n+1)
      Here -> (pc + fromIntegral n,bs,n)

