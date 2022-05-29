
-- | Semantics of z-machine operations.
module Semantics (theEffect) where

import Data.Bits ((.&.),shiftR)
import Decode (makeTarget)
import Dictionary (Dict(..))
import Eff (Eff(..),Bin(..))
import Header (Header(..))
import Numbers (Byte,Value,Addr,byteOfValue,addrOfPackedWord)
import Objects (FamilyMember(Parent,Sibling,Child))
import Operation (Operation,RoutineHeader,Func(..),Arg(..),Target(..),Label(..),Dest(..))
import Text.Printf (printf)
import qualified Data.Char as Char (chr) -- TODO: move to effect?
import qualified Objects
import qualified Operation as Op

type Effect a b s v x = Eff Addr Byte s Value x -- TODO: generalise completely!

theEffect :: Effect a b s v ()
theEffect = loop
  where
    loop = do
      pc <- GetPC -- only in case we fail to decode
      i <- FetchI
      eval pc i
      loop

eval :: Addr -> Operation -> Effect a b s v ()
eval pc = \case

  Op.BadOperation mes -> do
    error (printf "At [%s] %s" (show pc) mes)

  Op.Add arg1 arg2 target -> do evalBin BAdd arg1 arg2 target
  Op.And arg1 arg2 target -> do evalBin BAnd arg1 arg2 target

  Op.Call func args target -> do
    funcAddress <- evalFunc func
    if funcAddress == 0 then setTarget target 0 else do
      actuals <- mapM evalArg args
      PushFrame funcAddress target
      rh <- FetchRoutineHeader
      setLocals rh actuals

  Op.Clear_attr arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    Objects.clearAttr v1 v2

  Op.Dec arg -> do
    target <- makeValueTarget <$> evalArg arg
    v <- evalTarget target
    setTarget target (v - 1)

  Op.Dec_chk arg1 arg2 label -> do
    target <- makeValueTarget <$> evalArg arg1
    v1 <- evalTarget target
    v2 <- evalArg arg2
    let res = (v1 <= v2)
    setTarget target (v1 - 1)
    branchMaybe label res

  Op.Div arg1 arg2 target -> do evalBin BDiv arg1 arg2 target

  Op.Get_child arg target label -> do
    v <- evalArg arg
    res <- Objects.getFM Child v
    setTarget target res
    branchMaybe label (res /= 0)

  Op.Get_next_prop arg1 arg2 target -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- Objects.getNextProp v1 v2
    setTarget target res

  Op.Get_parent arg target -> do
    v <- evalArg arg
    res <- Objects.getFM Parent v
    setTarget target res

  Op.Get_prop arg1 arg2 target -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- Objects.getProp v1 v2
    setTarget target res

  Op.Get_prop_addr arg1 arg2 target -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- Objects.getPropAddr v1 v2
    setTarget target res

  Op.Get_prop_len arg target -> do
    v1 <- evalArg arg
    res <- Objects.getPropLen v1
    setTarget target res

  Op.Get_sibling arg target label -> do
    v <- evalArg arg
    res <- Objects.getFM Sibling v
    setTarget target res
    branchMaybe label (res /= 0)

  Op.Inc arg -> do
    target <- makeValueTarget <$> evalArg arg
    v <- evalTarget target
    setTarget target (v + 1)

  Op.Inc_chk arg1 arg2 label -> do
    target <- makeValueTarget <$> evalArg arg1
    v1 <- evalTarget target
    v2 <- evalArg arg2
    setTarget target (v1 + 1)
    branchMaybe label (v1 >= v2)

  Op.Insert_obj arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    Objects.insertObj v1 v2

  Op.Je args label -> do
    mapM evalArg args >>= EqualAny >>= branchMaybe label

  Op.Jg arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    branchMaybe label (v1 > v2)

  Op.Jin arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    p <- Objects.getFM Parent v1
    branchMaybe label (v2 == p)

  Op.Jl arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    branchMaybe label (v1 < v2)

  Op.Jump addr -> do SetPC addr
  Op.Jz arg label -> do evalArg arg >>= IsZero >>= branchMaybe label

  Op.Load arg target -> do
    var <- makeValueTarget <$> evalArg arg
    v <- evalTarget var
    case var of
      Sp{} -> error "TODO: re-push value on stack!"
      _ -> pure ()
    setTarget target v

  Op.Loadb arg1 arg2 target -> do
    base <- evalArg arg1
    offset <- evalArg arg2
    b <- GetByte (fromIntegral (base + offset))
    setTarget target (fromIntegral b)

  Op.Loadw arg1 arg2 target -> do
    base <- evalArg arg1
    offset <- evalArg arg2
    let a = fromIntegral base + 2 * fromIntegral offset
    w <- getWord a
    setTarget target w

  Op.Mul arg1 arg2 target -> do evalBin BMul arg1 arg2 target

  Op.New_line -> do LitS "\n" >>= GamePrint
  Op.Print string -> do LitS string >>= GamePrint

  Op.Print_addr arg -> do
    v <- evalArg arg
    let a :: Addr = fromIntegral v
    s <- GetText a
    GamePrint s

  Op.Print_char arg -> do
    v <- evalArg arg
    let c :: Char = Char.chr (fromIntegral v) -- TODO -- check char in bound!
    s <- LitS [c]
    GamePrint s

  Op.Print_num arg -> do
    v <- evalArg arg
    s <- LitS (show v)
    GamePrint s

  Op.Print_obj arg -> do
    v <- evalArg arg
    shortName <- Objects.getShortName v
    GamePrint shortName

  Op.Print_paddr arg -> do
    v <- evalArg arg
    let a :: Addr = addrOfPackedWord v
    s <- GetText a
    GamePrint s

  Op.Print_ret string -> do
    s <- LitS (string ++ "\n")
    GamePrint s
    returnValue 1

  Op.Pull arg -> do
    target <- makeValueTarget <$> evalArg arg
    v1 <- PopStack
    setTarget target v1

  Op.Push arg -> do evalArg arg >>= PushStack

  Op.Put_prop arg1 arg2 arg3 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    v3 <- evalArg arg3
    Objects.putProp v1 v2 v3

  Op.Quit -> do
    Quit

  Op.Random arg target -> do
    v1 <- evalArg arg
    res <- Random (fromIntegral v1)
    --Debug ("random",v1,"->",res)
    setTarget target (fromIntegral res)

  Op.Remove_obj arg -> do
    v <- evalArg arg
    Objects.removeObj v

  Op.Restart -> do -- TODO: implementation is not stack safe!
    Header{initialPC} <- StoryHeader
    SetPC initialPC

  Op.Ret_popped -> do PopStack >>= returnValue

  Op.Ret arg -> do
    v <- evalArg arg
    target <- PopFrame -- TODO: share code with returnValue
    setTarget target v

  Op.Rfalse -> do returnValue 0
  Op.Rtrue -> do returnValue 1

  Op.Set_attr arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    Objects.setAttr v1 v2

  Op.Sread arg1 arg2 -> do
    v0 <- evalGlobal 0
    v1 <- evalGlobal 1
    v2 <- evalGlobal 2
    p1 <- Objects.getShortName v0
    p2 <- LitS $ printf "score:%s--turns:%s" (show v1) (show v2)
    rawTyped <- ReadInputFromUser (p1,p2)
    t_buf :: Addr <- fromIntegral <$> evalArg arg1
    p_buf :: Addr <- fromIntegral <$> evalArg arg2
    Header{dictionary=dictBase} <- StoryHeader
    Dict{seps,entryLength,strings=constStrings} <- FetchDict
    strings <- mapM LitS constStrings
    -- +4 : #seps byte, entryLength byte, #entries word
    let baseEntries :: Addr = dictBase + fromIntegral (length seps + 4)
    (positionedWords,canoicalizedTyped) <- Tokenize rawTyped

    StringBytes canoicalizedTyped >>= writeBytes (t_buf+1)

    -- TODO: Process the VEC some kind of `ForEach' effect here?
    quads <- sequence
      [
        do
          iopt <- LookupInStrings strings word
          let dictAddr :: Addr =
                case iopt of
                  Just i -> baseEntries + fromIntegral ((i-1) * entryLength)
                  Nothing -> 0
          let (hi,lo) = splitWord (fromIntegral dictAddr)
          n <- StringLength word
          pure [ hi, lo, fromIntegral n, (fromIntegral pos) ]

      | (pos,word) <- positionedWords
      ]
    let bs :: [Byte] = fromIntegral (length quads) : concat quads
    writeBytes (fromIntegral (p_buf + 1)) bs

  Op.Store arg1 arg2 -> do
    target <- makeValueTarget <$> evalArg arg1
    v2 <- evalArg arg2
    case target of
      Sp{} -> undefined (do _ <- PopStack; pure ()) -- TODO: (from niz) enable code when hit
      _ -> pure ()
    setTarget target v2

  Op.Storeb arg1 arg2 arg3 -> do
    base <- evalArg arg1
    offset <- evalArg arg2
    value <- byteOfValue <$> evalArg arg3
    let a :: Addr = fromIntegral (base + offset)
    SetByte a value

  Op.Storew arg1 arg2 arg3 -> do
    base <- evalArg arg1
    offset <- evalArg arg2
    value <- evalArg arg3
    let a :: Addr = fromIntegral (base + 2 * offset)
    setWord a value

  Op.Sub arg1 arg2 target -> do evalBin BSub arg1 arg2 target

  Op.Test arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    let res = v1 .&. v2 == v2
    branchMaybe label res

  Op.Test_attr arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- Objects.testAttr v1 v2
    branchMaybe label res

  Op.Input_stream{} -> undefined
  Op.Mod{} -> undefined
  Op.Nop -> undefined
  Op.Or{} -> undefined
  Op.Output_stream{} -> undefined
  Op.Pop -> undefined
  Op.Restore{} -> undefined
  Op.Save{} -> undefined
  Op.Set_window{} -> undefined
  Op.Show_status -> undefined
  Op.Split_window{} -> undefined
  Op.Verify{} -> undefined


writeBytes :: Addr -> [Byte] -> Effect a b s v ()
writeBytes a bs = sequence_ [ SetByte (a+i) b | (i,b) <- zip [0..] bs ]

makeValueTarget :: Value -> Target
makeValueTarget = makeTarget . byteOfValue

evalBin :: Bin -> Arg -> Arg -> Target -> Effect a b s v ()
evalBin bin arg1 arg2 target = do
  v1 <- evalArg arg1
  v2 <- evalArg arg2
  v <- BinOp bin v1 v2
  setTarget target v

branchMaybe :: Label -> Bool -> Effect a b s v ()
branchMaybe (Branch sense dest) b = case (sense,b) of
  (Op.T,False) -> pure ()
  (Op.F,True) -> pure ()
  (Op.T,True) -> gotoDest dest
  (Op.F,False) -> gotoDest dest

gotoDest :: Dest -> Effect a b s v ()
gotoDest = \case
  Dfalse -> returnValue 0
  Dtrue -> returnValue 1
  Dloc a -> SetPC a

evalFunc :: Func -> Effect a b s v Addr
evalFunc = \case
  BadFunc -> error "failed to decode called function"
  Floc a -> pure a
  Fvar var -> do
    v <- evalTarget var
    let a = addrOfPackedWord v
    --Debug ("evalFunc/var",a) -- TODO: show dynamically reachable code
    pure a

evalArg :: Arg -> Effect a b s v Value
evalArg = \case
  Con x -> pure x
  Var v -> evalTarget v

evalTarget :: Target -> Effect a b s v Value
evalTarget = \case
  Sp -> PopStack
  Local n -> GetLocal n
  Global b -> evalGlobal b

evalGlobal :: Byte -> Effect a b s v Value
evalGlobal b = globalAddr b >>= getWord

returnValue :: Value -> Effect a b s v ()
returnValue v = do
  target <- PopFrame
  setTarget target v

setTarget :: Target -> Value -> Effect a b s v ()
setTarget var v = case var of
  Sp -> PushStack v
  Local n -> SetLocal n v
  Global b -> do
    a <- globalAddr b
    setWord a v

globalAddr :: Byte -> Effect a b s v Addr
globalAddr b = do
  Header{globalVars} <- StoryHeader
  pure (globalVars + 2 * fromIntegral b)

setLocals :: RoutineHeader -> [Value] -> Effect a b s v ()
setLocals rh actuals =
  case rh of
    Op.BadRoutineHeader -> error "setLocals: BadRoutineHeader, n>15"
    Op.RoutineHeader defs -> do
      -- TODO: we can do better here... !
      sequence_ [ SetLocal n v | (n,v) <- zip [1..] defs ]
      sequence_ [ SetLocal n v | (n,v) <- zip [1..] actuals ]

getWord :: Addr -> Effect a b s v Value -- TODO: make primitive
getWord a = do
  hi <- GetByte a
  lo <- GetByte (a+1)
  pure (256 * fromIntegral hi + fromIntegral lo)

setWord :: Addr -> Value -> Effect a b s v ()
setWord a w = do
  let (hi,lo) = splitWord w
  SetByte a hi
  SetByte (a+1) lo

splitWord :: Value -> (Byte,Byte)
splitWord w = do
  let hi = fromIntegral (w `shiftR` 8)
  let lo = fromIntegral (w .&. 0xff)
  (hi,lo)
