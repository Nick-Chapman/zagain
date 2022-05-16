
module Evaluation (theEffect) where

import Data.Bits ((.&.),shiftR)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Decode (makeTarget)
import Dictionary (Dict(..))
import Eff (Eff(..),Bin(..))
import Instruction (Instruction,RoutineHeader,Func(..),Args(..),Arg(..),Target(..),Label(..),Dest(..))
import Numbers (Byte,Value,Addr,byteOfValue,addrOfPackedWord)
import qualified Data.Char as Char (chr,ord)
import qualified Instruction as I
import qualified Objects

theEffect :: Eff ()
theEffect = loop
  where
    loop = do
      i <- FetchI
      eval i
      loop

eval :: Instruction -> Eff ()
eval = \case

  I.Add arg1 arg2 target -> do evalBin BAdd arg1 arg2 target
  I.And_ arg1 arg2 target -> do evalBin BAnd arg1 arg2 target

  I.Call func (Args args) target -> do
    funcAddress <- evalFunc func
    --Debug ("Call",funcAddress)
    if funcAddress == 0 then setTarget target 0 else do
      actuals <- mapM evalArg args
      PushFrame funcAddress target
      rh <- FetchHeader
      setLocals rh actuals

  I.Clear_attr arg1 arg2 -> do
    let _ = undefined arg1 arg2
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    Objects.clearAttr (fromIntegral v1) (fromIntegral v2)

  I.Dec arg -> do
    target <- makeValueTarget <$> evalArg arg
    v <- evalTarget target
    setTarget target (v - 1)

  I.Dec_check arg1 arg2 label -> do
    target <- makeValueTarget <$> evalArg arg1
    v1 <- evalTarget target
    v2 <- evalArg arg2
    let res = (v1 <= v2)
    --Debug ("Dec_check",(arg1,v1),(arg2,v2),target,label,res)
    setTarget target (v1 - 1)
    branchMaybe label res

  I.Div arg1 arg2 target -> do evalBin BDiv arg1 arg2 target

  I.Get_child arg target label -> do
    v <- evalArg arg
    res <- Objects.getChild (fromIntegral v)
    setTarget target (fromIntegral res)
    branchMaybe label (res /= 0)

  I.Get_next_prop arg1 arg2 target -> do
    undefined arg1 arg2 target

  I.Get_parent arg target -> do
    v <- evalArg arg
    res <- Objects.getParent (fromIntegral v)
    setTarget target (fromIntegral res)

  I.Get_prop arg1 arg2 target -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- Objects.getProp (fromIntegral v1) (fromIntegral v2)
    --Debug ("Get_prop",v1,v2,"-->",res)
    setTarget target res

  I.Get_prop_addr arg1 arg2 target -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- Objects.getPropAddr (fromIntegral v1) (fromIntegral v2)
    --Debug ("Get_prop_addr",v1,"-->",res)
    setTarget target res

  I.Get_prop_len arg target -> do
    v1 <- evalArg arg
    res <- Objects.getPropLen v1
    --Debug ("Get_prop_len",v1,"-->",res)
    setTarget target res

  I.Get_sibling arg target label -> do
    v <- evalArg arg
    res <- Objects.getSibling (fromIntegral v)
    setTarget target (fromIntegral res)
    let bFix = (res /= 0)
    branchMaybe label bFix

  I.Inc arg -> do
    target <- makeValueTarget <$> evalArg arg
    v <- evalTarget target
    setTarget target (v + 1)

  I.Inc_check arg1 arg2 label -> do
    target <- makeValueTarget <$> evalArg arg1
    v1 <- evalTarget target
    v2 <- evalArg arg2
    --Debug("Inc_check",v1,v2)
    setTarget target (v1 + 1)
    branchMaybe label (v1 >= v2)

  I.Insert_obj arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    Objects.insertObj (fromIntegral v1) (fromIntegral v2)
    pure ()

  I.Je (Args args) label -> do
    mapM evalArg args >>= EqualAny >>= branchMaybe label

  I.Jg arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    branchMaybe label (v1 > v2)

  I.Jin arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    p <- fromIntegral <$> Objects.getParent (fromIntegral v1)
    branchMaybe label (v2 == p)

  I.Jl arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    branchMaybe label (v1 < v2)

  I.Jump addr -> do SetPC addr
  I.Jz arg label -> do evalArg arg >>= IsZero >>= branchMaybe label

  I.Load_byte arg1 arg2 target -> do
    base <- evalArg arg1
    offset <- evalArg arg2
    --Debug ("Load_byte",base,offset) -- TODO: GetByte(0,1) differs from mojo
    b <- GetByte (fromIntegral (base + offset))
    setTarget target (fromIntegral b)

  I.Load_word arg1 arg2 target -> do
    base <- evalArg arg1
    offset <- evalArg arg2
    w <- getWord (fromIntegral (base + 2*offset))
    setTarget target w

  I.Mul arg1 arg2 target -> do evalBin BMul arg1 arg2 target

  I.New_line -> do GamePrint "\n"
  I.Print string -> do GamePrint string

  I.Print_addr arg -> do
    v <- evalArg arg
    let a :: Addr = fromIntegral v
    s <- GetText a
    GamePrint s

  I.Print_char arg -> do
    v <- evalArg arg
    let c :: Char = Char.chr (fromIntegral v) -- TODO -- check char in bound!
    GamePrint [c]

  I.Print_num arg -> do evalArg arg >>= GamePrint . show

  I.Print_obj arg -> do
    v <- evalArg arg
    shortName <- Objects.getShortName (fromIntegral v)
    GamePrint shortName

  I.Print_paddr arg -> do
    v <- evalArg arg
    let a :: Addr = addrOfPackedWord v
    s <- GetText a
    GamePrint s

  I.Print_ret string -> do GamePrint (string ++ "\n"); returnValue 1

  I.Pull arg -> do
    target <- makeValueTarget <$> evalArg arg
    v1 <- PopStack
    setTarget target v1

  I.Push arg -> do evalArg arg >>= PushStack

  I.Put_prop arg1 arg2 arg3 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    v3 <- evalArg arg3
    --Debug ("TODO: Put_prop",v1,v2,v3)
    Objects.putProp (fromIntegral v1) (fromIntegral v2) v3

  I.Random arg target -> do
    v1 <- evalArg arg
    let res = 1
    Debug("TODO:Random",v1,"--(fixed)-->",res)
    setTarget target res

  I.Ret_popped -> do PopStack >>= returnValue
  I.Return arg -> do
    v <- evalArg arg
    target <- PopFrame
    setTarget target v

  I.Rfalse -> do returnValue 0
  I.Rtrue -> do returnValue 1

  I.Set_attr arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    Objects.setAttr (fromIntegral v1) (fromIntegral v2)

  I.Sread arg1 arg2 -> do
    rawTyped <- ReadInputFromUser

    t_buf :: Addr <- fromIntegral <$> evalArg arg1
    p_buf :: Addr <- fromIntegral <$> evalArg arg2

    --Debug ("Sread",(arg1,t_buf),(arg2,p_buf),"-->",rawTyped)

    dictBase :: Addr <- fromIntegral <$> getWord 0x8 -- TODO header
    Dict{seps,entryLength,strings} <- FetchDict
    -- +4 : #seps byte, entryLength byte, #entries word
    let baseEntries :: Addr = dictBase + fromIntegral (length seps + 4)

    let
      mkQuad :: Int -> String -> [Byte]
      mkQuad offsetInText word = do
        let
          iopt :: Maybe Int = do
            let key = take 6 word
            case [ i | (i,s) <- zip [1..] strings, s == key ] of
              [] -> Nothing
              xs@(_:_:_) -> error (show ("multi dict match!",word,xs))
              [i] -> Just i
        let dictAddr :: Addr =
              case iopt of
                Just i -> baseEntries + fromIntegral ((i-1) * entryLength)
                Nothing -> 0
        let (hi,lo) = splitWord (fromIntegral dictAddr)
        let quad1 :: [Byte] = [ hi, lo, fromIntegral (length word), (fromIntegral offsetInText) ]
        quad1

    let words = [ w | w <- splitOn " " rawTyped, w /= "" ]
    --Debug words

    let
      offsets = do
        let lens = [ length w | w <- words ]
        let
          f :: (Int,[Int]) -> Int -> (Int,[Int])
          f (off,xs) i = (off+i+1, off : xs) --
        let z = (1,[])
        let (_,offsetsR) = foldl f z lens
        reverse offsetsR

    --Debug offsets

    let canoicalizedTyped = intercalate " " words  ++ "\0"
    --Debug ("canoicalizedTyped",canoicalizedTyped)

    let positionedWords = zip offsets words
    --Debug positionedWords

    let quads = [ mkQuad pos word | (pos,word) <- positionedWords ]
    let bs :: [Byte] = fromIntegral (length quads) : concat quads
    --Debug (length bs, bs)

    writeBytesFromString (t_buf+1) canoicalizedTyped
    writeBytes (fromIntegral (p_buf + 1)) bs
    pure ()

  I.Store arg1 arg2 -> do
    target <- makeValueTarget <$> evalArg arg1
    v2 <- evalArg arg2
    case target of
      Sp{} -> undefined (do _ <- PopStack; pure ()) -- from niz
      _ -> pure ()
    setTarget target v2

  I.Storeb arg1 arg2 arg3 -> do
    base <- evalArg arg1
    offset <- evalArg arg2
    value <- byteOfValue <$> evalArg arg3
    --Debug ("xx",base,offset,value)
    let a :: Addr = fromIntegral (base + offset)
    SetByte a value

  I.Storew arg1 arg2 arg3 -> do
    base <- evalArg arg1
    offset <- evalArg arg2
    value <- evalArg arg3
    let a :: Addr = fromIntegral (base + 2 * offset)
    setWord a value

  I.Sub arg1 arg2 target -> do evalBin BSub arg1 arg2 target

  I.Test arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    let res = v1 .&. v2 == v2
    branchMaybe label res

  I.Test_attr arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- Objects.testAttr (fromIntegral v1) (fromIntegral v2)
    branchMaybe label res


writeBytesFromString :: Addr -> String -> Eff ()
writeBytesFromString a str = writeBytes a [ fromIntegral (Char.ord c) | c <- str ]

writeBytes :: Addr -> [Byte] -> Eff ()
writeBytes a bs = sequence_ [ SetByte (a+i) b | (i,b) <- zip [0..] bs ]

makeValueTarget :: Value -> Target
makeValueTarget = makeTarget . byteOfValue


evalBin :: Bin -> Arg -> Arg -> Target -> Eff ()
evalBin bin arg1 arg2 target = do
  v1 <- evalArg arg1
  v2 <- evalArg arg2
  v <- BinOp bin v1 v2
  setTarget target v

branchMaybe :: Label -> Bool -> Eff ()
branchMaybe (Branch sense dest) b = case (sense,b) of
  (I.T,False) -> pure ()
  (I.F,True) -> pure ()
  (I.T,True) -> gotoDest dest
  (I.F,False) -> gotoDest dest

gotoDest :: Dest -> Eff ()
gotoDest = \case
  Dfalse -> returnValue 0
  Dtrue -> returnValue 1
  Dloc a -> SetPC a

evalFunc :: Func -> Eff Addr
evalFunc = \case
  Floc a -> pure a
  Fvar var -> do
    v <- evalTarget var
    let a = addrOfPackedWord v
    --Debug ("evalFunc/var",a) -- TODO: show dynamically reachable code
    pure $ a

evalArg :: Arg -> Eff Value
evalArg = \case
  Con x -> pure x
  Var v -> evalTarget v

evalTarget :: Target -> Eff Value
evalTarget = \case
  Sp -> PopStack
  Local n -> GetLocal n
  Global b -> do
    a <- globalAddr b
    getWord a

returnValue :: Value -> Eff ()
returnValue v = do
  target <- PopFrame
  setTarget target v


setTarget :: Target -> Value -> Eff ()
setTarget var v = case var of
  Sp -> PushStack v
  Local n -> SetLocal n v
  Global b -> do
    a <- globalAddr b
    setWord a v

globalAddr :: Byte -> Eff Addr
globalAddr b = do
  globalBase :: Value <- getWord 0xC
  pure $ fromIntegral (globalBase + 2 * fromIntegral b)

setLocals :: RoutineHeader -> [Value] -> Eff ()
setLocals (I.RoutineHeader defs) actuals = do
  sequence_ [ SetLocal n v | (n,v) <- zip [1..] defs ]
  sequence_ [ SetLocal n v | (n,v) <- zip [1..] actuals ]

getWord :: Addr -> Eff Value
getWord a = do
  hi <- GetByte a
  lo <- GetByte (a+1)
  pure (256 * fromIntegral hi + fromIntegral lo)

setWord :: Addr -> Value -> Eff ()
setWord a w = do
  let (hi,lo) = splitWord w
  SetByte a hi
  SetByte (a+1) lo

splitWord :: Value -> (Byte,Byte)
splitWord w = do
  let hi = fromIntegral (w `shiftR` 8)
  let lo = fromIntegral (w .&. 0xff)
  (hi,lo)
