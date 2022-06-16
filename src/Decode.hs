
-- | Decode z-machine instructions, routine-headers, and z-text from a story-file.
module Decode
  ( fetchOperation
  , fetchRoutineHeader
  , makeTarget
  , ztext
  ) where

import Data.Array ((!),listArray)
import Data.Bits (testBit,(.&.),(.|.),shiftL,shiftR)
import Fetch (Fetch(..))
import Header (Header(..))
import Numbers (Zversion(..),Byte,Addr,Value,makeWordAddress,makePackedAddress,makeHiLo)
import Operation (Operation,Func(..),Arg(..),Target(..),Label(..),Dest(..),Sense(T,F),RoutineHeader(..))
import Text.Printf (printf)
import qualified Data.Char as Char
import qualified Operation as Op

data OpCodeAndArgs = Code Byte [RandType]
  deriving Show

data RandType = ByteConst | WordConst | ByteVariable
  deriving Show

fetchOperation :: Fetch Operation
fetchOperation = do
  Header{zv} <- StoryHeader
  fetchOpCodeAndArgs >>= decode zv

decode :: Zversion -> OpCodeAndArgs -> Fetch Operation
decode zv op = case op of

  Code 1 ts -> Op.Je <$> mapM arg ts <*> label
  Code 2 [t1,t2] -> Op.Jl <$> arg t1 <*> arg t2 <*> label
  Code 3 [t1,t2] -> Op.Jg <$> arg t1 <*> arg t2 <*> label
  Code 4 [t1,t2] -> Op.Dec_chk <$> arg t1 <*> arg t2 <*> label
  Code 5 [t1,t2] -> Op.Inc_chk <$> arg t1 <*> arg t2 <*> label
  Code 6 [t1,t2] -> Op.Jin <$> arg t1 <*> arg t2 <*> label
  Code 7 [t1,t2] -> Op.Test <$> arg t1 <*> arg t2 <*> label
  Code 8 [t1,t2] -> Op.Or <$> arg t1 <*> arg t2 <*> target
  Code 9 [t1,t2] -> Op.And <$> arg t1 <*> arg t2 <*> target
  Code 10 [t1,t2] -> Op.Test_attr <$> arg t1 <*> arg t2 <*> label
  Code 11 [t1,t2] -> Op.Set_attr <$> arg t1 <*> arg t2
  Code 12 [t1,t2] -> Op.Clear_attr <$> arg t1 <*> arg t2
  Code 13 [t1,t2] -> Op.Store <$> arg t1 <*> arg t2
  Code 14 [t1,t2] -> Op.Insert_obj <$> arg t1 <*> arg t2
  Code 15 [t1,t2] -> Op.Loadw <$> arg t1 <*> arg t2 <*> target
  Code 16 [t1,t2] -> Op.Loadb <$> arg t1 <*> arg t2 <*> target
  Code 17 [t1,t2] -> Op.Get_prop <$> arg t1 <*> arg t2 <*> target
  Code 18 [t1,t2] -> Op.Get_prop_addr <$> arg t1 <*> arg t2 <*> target
  Code 19 [t1,t2] -> Op.Get_next_prop <$> arg t1 <*> arg t2 <*> target
  Code 20 [t1,t2] -> Op.Add <$> arg t1 <*> arg t2 <*> target
  Code 21 [t1,t2] -> Op.Sub <$> arg t1 <*> arg t2 <*> target
  Code 22 [t1,t2] -> Op.Mul <$> arg t1 <*> arg t2 <*> target
  Code 23 [t1,t2] -> Op.Div <$> arg t1 <*> arg t2 <*> target
  Code 24 [t1,t2] -> Op.Mod <$> arg t1 <*> arg t2 <*> target

  Code 25 [t1,t2]
    | zv>=Z4 -> do Op.Call <$> func zv t1 <*> mapM arg [t2] <*> target

  Code 26 [t1,t2]
    | zv>=Z5 -> do Op.CallN <$> func zv t1 <*> mapM arg [t2]

  Code 27 _ -> bad op --Z5,Z6
  Code 28 _ -> bad op --Z5,Z6
  Code 29 _ -> bad op
  Code 30 _ -> bad op
  Code 31 _ -> bad op

  Code n ts | 32<=n && n<=127 -> decode zv (Code (n .&. 0x1f) ts)

  Code 128 [t] -> Op.Jz <$> arg t <*> label
  Code 129 [t] -> Op.Get_sibling <$> arg t <*> target <*> label
  Code 130 [t] -> Op.Get_child <$> arg t <*> target <*> label
  Code 131 [t] -> Op.Get_parent <$> arg t <*> target
  Code 132 [t] -> Op.Get_prop_len <$> arg t <*> target
  Code 133 [t] -> Op.Inc <$> arg t
  Code 134 [t] -> Op.Dec <$> arg t
  Code 135 [t] -> Op.Print_addr <$> arg t
  Code 136 [t] | zv>=Z4 -> flip Op.Call [] <$> func zv t <*> target
  Code 137 [t] -> Op.Remove_obj <$> arg t
  Code 138 [t] -> Op.Print_obj <$> arg t
  Code 139 [t] -> Op.Ret <$> arg t
  Code 140 [WordConst] -> Op.Jump <$> jumpLocation
  Code 141 [t] -> Op.Print_paddr <$> arg t
  Code 142 [t] -> Op.Load <$> arg t <*> target

  Code 143 [t] | zv>=Z5 -> do flip Op.CallN [] <$> func zv t
  Code 143 _ -> bad op --Z1,Z4

  Code n ts | 144<=n && n<=175 -> decode zv (Code (n .&. 0x8f) ts)

  Code 176 [] -> pure Op.Rtrue
  Code 177 [] -> pure Op.Rfalse
  Code 178 [] -> Op.Print <$> ztext
  Code 179 [] -> Op.Print_ret <$> ztext
  Code 180 [] -> pure Op.Nop
  Code 181 [] -> Op.Save <$> label -- changes in Z4/Z5
  Code 182 [] -> Op.Restore <$> label -- changes in Z4/Z5
  Code 183 [] -> pure Op.Restart
  Code 184 [] -> pure Op.Ret_popped
  Code 185 [] -> pure Op.Pop -- changes in Z5/Z6
  Code 186 [] -> pure Op.Quit
  Code 187 [] -> pure Op.New_line
  Code 188 [] -> pure Op.Show_status -- illegal from Z4
  Code 189 [] -> Op.Verify <$> label

  Code 190 args -> do x <- NextByte; decodeExtended zv (Code x args)

  Code 191 _ -> bad op --Z5

  Code n ts | 192<=n && n<=223 -> decode zv (Code (n .&. 0x1f) ts)

  Code 224 (t1:ts) -> Op.Call <$> func zv t1 <*> mapM arg ts <*> target
  Code 225 [t1,t2,t3] -> Op.Storew <$> arg t1 <*> arg t2 <*> arg t3
  Code 226 [t1,t2,t3] -> Op.Storeb <$> arg t1 <*> arg t2 <*> arg t3
  Code 227 [t1,t2,t3] -> Op.Put_prop <$> arg t1 <*> arg t2 <*> arg t3

  Code 228 [t1,t2]
    | zv<=Z4 -> Op.Sread <$> arg t1 <*> arg t2
    | otherwise -> Op.Aread <$> arg t1 <*> arg t2 <*> target

  Code 229 [t] -> Op.Print_char <$> arg t
  Code 230 [t] -> Op.Print_num <$> arg t
  Code 231 [t] -> Op.Random <$> arg t <*> target
  Code 232 [t] -> Op.Push <$> arg t
  Code 233 [t] -> Op.Pull <$> arg t -- changes in Z6
  Code 234 [t] -> Op.Split_window <$> arg t
  Code 235 [t] -> Op.Set_window <$> arg t

  Code 236 (t1:ts)
    | zv>=Z5 -> do Op.Call <$> func zv t1 <*> mapM arg ts <*> target

  Code 237 [t] | zv>=Z4 -> Op.Erase_window <$> arg t
  -- 238
  Code 239 [t1,t2] | zv>=Z4 -> Op.Set_cursor <$> arg t1 <*> arg t2
  -- 240
  Code 241 [t] | zv>=Z4 -> Op.Set_text_style <$> arg t
  Code 242 [t] | zv>=Z4 -> Op.Buffer_mode <$> arg t
  Code 243 [t] -> Op.Output_stream1 <$> arg t
  Code 243 [t1,t2] -> Op.Output_stream2 <$> arg t1 <*> arg t2
  Code 244 [t] -> Op.Input_stream <$> arg t
  Code 245 [t1,t2,t3] -> Op.Sound_effect <$> arg t1 <*> arg t2 <*> arg t3
  Code 246 [_ignored_mustBe1] -> Op.Read_char <$> target
  Code 247 [t1,t2,t3]
    | zv>=Z4 -> Op.Scan_table <$> arg t1 <*> arg t2 <*> arg t3 <*> target <*> label

  Code 248 [t] | zv>=Z5 -> Op.Not <$> arg t <*> target
  Code 249 (t1:ts) | zv>=Z5 -> Op.CallN <$> func zv t1 <*> mapM arg ts
  Code 250 (t1:ts) | zv>=Z5 -> Op.CallN <$> func zv t1 <*> mapM arg ts
  Code 251 [t1,t2] | zv>=Z5 -> Op.Tokenize <$> arg t1 <*> arg t2
  Code 255 [t] | zv>=Z5 -> Op.Check_arg_count <$> arg t <*> label

  _ -> bad op


decodeExtended :: Zversion -> OpCodeAndArgs -> Fetch Operation
decodeExtended _zv = \case
  Code 9 [] -> Op.Save_undo <$> label
  op -> bad op


bad :: OpCodeAndArgs -> Fetch Operation
bad x = pure $ Op.BadOperation $ printf "unexpected op-code/args: %s" (show x)


data Form = LongForm | ShortForm | VarForm

fetchOpCodeAndArgs :: Fetch OpCodeAndArgs
fetchOpCodeAndArgs = do
  x <- Fetch.NextByte
  case decodeForm x of
    LongForm -> do
      let t1 = decodeLongRandType x 6
      let t2 = decodeLongRandType x 5
      pure (Code x [t1,t2])
    ShortForm -> do
      case decodeRandType x (5,4) of
        Nothing -> pure (Code x [])
        Just ty -> pure (Code x [ty])
    VarForm -> do
      x2 <- Fetch.NextByte
      let tys = decodeRandTypes x2
      case x == 236 || x == 250 of
        -- two ops have a 2nd byte of opcode types, allowing up to 7 args
        True -> do
          x3 <- Fetch.NextByte
          let tys' = decodeRandTypes x3
          pure (Code x (tys ++ tys'))
        False -> do
          pure (Code x tys)

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

func :: Zversion -> RandType -> Fetch Func
func zv = \case
  ByteConst -> pure BadFunc
  WordConst -> (Floc . makePackedAddress zv) <$> fetchNextWord
  ByteVariable -> (Fvar . makeTarget) <$> Fetch.NextByte

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
  pure $ makeHiLo hi lo


fetchRoutineHeader :: Fetch RoutineHeader
fetchRoutineHeader = do
  n <- Fetch.NextByte
  if n > 15 then pure BadRoutineHeader else do
    ws <- do
      Header{zv} <- StoryHeader
      let getDefaultValue = if
            | zv <= Z4 -> fetchNextWord
            | otherwise -> pure 0
      sequence (take (fromIntegral n) (repeat getDefaultValue))
    pure (RoutineHeader ws)

----------------------------------------------------------------------

getWord :: Addr -> Fetch Value
getWord a = do
  hi <- GetByte a
  lo <- GetByte (a+1)
  pure $ makeHiLo hi lo

ztext :: Fetch String
ztext = do Header{zv} <- StoryHeader; ztext' True zv

ztext' :: Bool -> Zversion -> Fetch String
ztext' allowAbbrev version = loop [] A0 A0 []
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

      1:zs | (version==Z1) ->
        inner alpha lock stop ('\n' : acc) zs

      2:zs | (version<=Z2) -> inner (shiftUp alpha) lock stop acc zs
      3:zs | (version<=Z2) -> inner (shiftDown alpha) lock stop acc zs

      [z] | z `elem` [1,2,3] -> loop [z] alpha lock acc

      n:z:zs | n `elem` [1,2,3] -> do
        expansion <-
          if allowAbbrev
          then decodeAbbrev (z + (n-1) * 32)
          else error "recursive abbrev"
        inner lock lock stop (reverse expansion ++ acc) zs

      4:zs | (version<=Z2) -> inner locked locked stop acc zs
        where locked = shiftUp alpha

      5:zs | (version<=Z2) -> inner locked locked stop acc zs
        where locked = shiftDown alpha

      4:zs -> inner (shiftUp alpha) lock stop acc zs
      5:zs -> inner (shiftDown alpha) lock stop acc zs

      6:a:b:zs | (alpha == A2) -> do
        let c = Char.chr (fromIntegral a * 32 + fromIntegral b)
        inner lock lock stop (c : acc) (zs)

      zs@(6:_) | (alpha == A2) -> loop zs alpha lock acc

      z:zs -> inner lock lock stop (decodeAlphabet version alpha (z - 6) : acc) zs


decodeAbbrev :: Value -> Fetch String
decodeAbbrev n = do
  Header{zv,abbrevTable} <- StoryHeader
  thisAbbrev :: Addr <- makeWordAddress <$> getWord (abbrevTable + fromIntegral (2 * n))
  WithPC thisAbbrev (ztext' False zv)


data Alpha = A0 | A1 | A2 deriving Eq

shiftUp :: Alpha -> Alpha
shiftUp = \case A0 -> A1; A1 -> A2; A2 -> A0

shiftDown :: Alpha -> Alpha
shiftDown = \case A0 -> A2; A1 -> A0; A2 -> A1

decodeAlphabet :: Zversion -> Alpha -> Value -> Char
decodeAlphabet version alpha i = if
  | (i < 0 || i > 25) -> error (show ("decodeAlphabet",i))
  | otherwise -> a ! i
  where
    a = case alpha of A0 -> a0; A1 -> a1; A2 -> a2
    a0 = listArray (0,25) "abcdefghijklmnopqrstuvwxyz"
    a1 = listArray (0,25) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    a2 = if
      | version==Z1 -> listArray (0,25) " 0123456789.,!?_#'\"/\\<-:()"
      | otherwise -> listArray (0,25) " \n0123456789.,!?_#'\"/\\-:()"
