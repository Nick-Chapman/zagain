
module Evaluation (theEffect) where

import Data.Bits ((.&.),shiftR)
import Decode (makeTarget)
import Eff (Eff(..),Bin(..))
import Instruction (Instruction,RoutineHeader,Func(..),Args(..),Arg(..),Target(..),Label(..),Dest(..))
import Numbers (Byte,Value,Addr,byteOfValue,addrOfPackedWord)
import Text.Printf (printf)
import qualified Data.Char as Char
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
  I.Bad s -> do error (show ("Bad",s))

  I.Add arg1 arg2 target -> do evalBin BAdd arg1 arg2 target
  I.And_ arg1 arg2 target -> do evalBin BAnd arg1 arg2 target

  I.Call func (Args args) target -> do
    funcAddress <- evalFunc func
    PushFrame funcAddress target
    rh <- FetchHeader
    actuals <- mapM evalArg args
    setLocals rh actuals

  I.Clear_attr arg1 arg2 -> do undefined arg1 arg2
  I.Dec arg -> do undefined arg
  I.Dec_check arg1 arg2 label -> do undefined arg1 arg2 label
  I.Div arg1 arg2 target -> do undefined arg1 arg2 target

  I.Get_child arg target label -> do
    v <- evalArg arg
    res <- Objects.getChild (fromIntegral v)
    setTarget target (fromIntegral res)
    branchMaybe label (v /= 0)

  I.Get_parent arg target -> do
    v <- evalArg arg
    res <- Objects.getParent (fromIntegral v)
    setTarget target (fromIntegral res)

  I.Get_prop arg1 arg2 target -> do
    let res = 19102
    let _ = Debug ("TODO:Get_prop(HACK fixed res)",arg1,arg2,target,res)
    setTarget target res
    pure ()

  I.Get_prop_addr arg1 arg2 target -> do
    let _ = Debug ("TODO:Get_prop_addr",arg1,arg2,target)
    pure ()

  I.Get_prop_len arg target -> do undefined arg target

  I.Get_sibling arg target label -> do
    v <- evalArg arg
    res <- Objects.getSibling (fromIntegral v)
    setTarget target (fromIntegral res)
    branchMaybe label (v /= 0)

  I.Inc arg -> do undefined arg

  I.Inc_check arg1 arg2 label -> do
    target <- makeValueTarget <$> evalArg arg1
    v1 <- evalTarget target
    v2 <- evalArg arg2
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
    let _ = Debug ("TODO: Jin",arg1,arg2,label)
    pure ()

  I.Jl arg1 arg2 label -> do undefined arg1 arg2 label

  I.Jump addr -> do SetPC addr
  I.Jz arg label -> do evalArg arg >>= IsZero >>= branchMaybe label

  I.Load_byte arg1 arg2 target -> do
    base <- evalArg arg1
    offset <- evalArg arg2
    b <- GetByte (fromIntegral (base + 2*offset))
    setTarget target (fromIntegral b)

  I.Load_word arg1 arg2 target -> do
    base <- evalArg arg1
    offset <- evalArg arg2
    w <- getWord (fromIntegral (base + 2*offset))
    setTarget target w

  I.Mul arg1 arg2 target -> do undefined arg1 arg2 target

  I.New_line -> do GamePrint "\n"
  I.Print string -> do GamePrint string

  I.Print_addr arg -> do undefined arg

  I.Print_char arg -> do
    v <- evalArg arg
    let c :: Char = Char.chr (fromIntegral v)
    -- TODO -- check char in bound!
    GamePrint [c]

  I.Print_num arg -> do evalArg arg >>= GamePrint . show

  I.Print_obj arg -> do
    v <- evalArg arg
    shortName <- getObjShortName v
    GamePrint shortName

  I.Print_paddr arg -> do undefined arg
  I.Print_ret string -> do undefined string

  I.Pull arg -> do
    target <- makeValueTarget <$> evalArg arg
    v1 <- PopStack
    setTarget target v1


  I.Push arg -> do evalArg arg >>= PushStack

  I.Put_prop arg1 arg2 arg3 -> do
    let _ = Debug ("TODO: Put_prop",arg1,arg2,arg3)
    pure ()

  I.Random arg target -> do undefined arg target
  I.Ret_popped -> do PopStack >>= returnValue
  I.Return arg -> do
    v <- evalArg arg
    target <- PopFrame
    setTarget target v

  I.Rfalse -> do returnValue 0
  I.Rtrue -> do returnValue 1

  I.Set_attr arg1 arg2 -> do
    let _ = Debug ("TODO: Set_attr",arg1,arg2)
    pure ()

  I.Sread arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    typed <- ReadInputFromUser
    Debug ("Sread",(arg1,v1),(arg2,v2),"-->",typed)

  I.Store arg1 arg2 -> do
    target <- makeValueTarget <$> evalArg arg1
    v2 <- evalArg arg2
    case target of
      Sp{} -> undefined (do _ <- PopStack; pure ()) -- from niz
      _ -> pure ()
    setTarget target v2
    pure ()

  I.Storeb arg1 arg2 arg3 -> do undefined arg1 arg2 arg3

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
    let res = False
    let _ = Debug ("TODO:Test_attr(hack res=FALSE)",arg1,arg2,label)
    branchMaybe label res

getObjShortName :: Value -> Eff String
getObjShortName v = do
  pure $ printf "object<%s>" (show v) --TODO


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
    pure $ addrOfPackedWord v

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
setTarget var v = do
  --Debug (show ("setTarget",var,v))
  setTarget' var v

setTarget' :: Target -> Value -> Eff ()
setTarget' var v = case var of
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
  let hi = fromIntegral (w `shiftR` 8)
  let lo = fromIntegral (w .&. 0xff)
  SetByte a hi
  SetByte (a+1) lo
