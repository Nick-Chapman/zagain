
-- | Semantics of z-machine operations.
module Semantics (smallStep) where

--import Dictionary (Dict(..))
import Eff (Eff(..),Phase(..),Mode,Control(..),StatusInfo(..))
import Header (Header(..))
import Numbers (Zversion(..))
import Objects (FamilyMember(Parent,Sibling,Child))
import Operation (Operation,RoutineHeader,Func(..),Arg(..),Target(..),Label(..),Dest(..))
import Text.Printf (printf)
import qualified Objects
import qualified Operation as Op

smallStep :: Phase p => Mode -> Eff p ()
smallStep mode = do
  GetControl >>= \case

    AtInstruction{pc=here} -> do
      (operation,pc) <- FetchOperation here
      SetControl AtInstruction { pc }
      TraceOperation here operation
      eval mode here operation

    AtRoutineHeader{routine,numActuals} -> do
      (rh,pc) <- FetchRoutineHeader routine
      setDefaults rh numActuals
      SetControl AtInstruction { pc }

    AtReturnFromCall{caller,result=v} -> do
      (operation,pc) <- FetchOperation caller
      setTarget (callTarget operation) v
      SetControl AtInstruction { pc }

    where
      callTarget = \case
        Op.Call _ _ target -> target
        _ ->  error "callTarget: not a call instruction!"


eval :: Phase p => Mode -> Addr p -> Operation -> Eff p ()
eval mode here = \case

  Op.BadOperation mes -> do
    error (printf "At [%s] %s" (show here) mes)

  Op.Add arg1 arg2 target -> do evalBin Add arg1 arg2 target
  Op.And arg1 arg2 target -> do evalBin And arg1 arg2 target

  Op.Call func args target -> do
    routine <- evalFunc func
    p <- IsZeroAddress routine >>= If
    if p then LitV 0 >>= setTarget target else do
      actuals <- mapM evalArg args
      PushFrame here
      setActuals actuals
      numActuals <- LitB $ fromIntegral (length actuals)
      TraceRoutineCall routine -- for dynamic discovery
      SetControl AtRoutineHeader { routine, numActuals }

  Op.Clear_attr arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    Objects.clearAttr v1 v2

  Op.Dec arg -> do
    dyn <- evalArgAsDyn arg
    v <- evalDyn dyn
    one <- LitV 1
    v' <- Sub v one
    setDyn dyn v'

  Op.Dec_chk arg1 arg2 label -> do
    dyn <- evalArgAsDyn arg1
    v1 <- evalDyn dyn
    v2 <- evalArg arg2
    res <- LessThanEqual v1 v2 >>= If
    one <- LitV 1
    v1' <- Sub v1 one
    setDyn dyn v1'
    branchMaybe label res

  Op.Div arg1 arg2 target -> do evalBin Div arg1 arg2 target

  Op.Get_child arg target label -> do
    v <- evalArg arg
    res <- Objects.getFM Child v
    setTarget target res
    b <- IsZero res >>= If
    branchMaybe label (not b)

  Op.Get_next_prop arg1 arg2 target -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- Objects.getNextProp mode v1 v2
    setTarget target res

  Op.Get_parent arg target -> do
    v <- evalArg arg
    res <- Objects.getFM Parent v
    setTarget target res

  Op.Get_prop arg1 arg2 target -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- Objects.getProp mode v1 v2
    setTarget target res

  Op.Get_prop_addr arg1 arg2 target -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- Objects.getPropAddr mode v1 v2
    setTarget target res

  Op.Get_prop_len arg target -> do
    v1 <- evalArg arg
    res <- Objects.getPropLen v1
    setTarget target res

  Op.Get_sibling arg target label -> do
    v <- evalArg arg
    res <- Objects.getFM Sibling v
    setTarget target res
    b <- IsZero res >>= If
    branchMaybe label (not b)

  Op.Inc arg -> do
    dyn <- evalArgAsDyn arg
    v <- evalDyn dyn
    one <- LitV 1
    v' <- Add v one
    setDyn dyn v'

  Op.Inc_chk arg1 arg2 label -> do
    dyn <- evalArgAsDyn arg1
    v1 <- evalDyn dyn
    v2 <- evalArg arg2
    one <- LitV 1
    v1' <- Add v1 one
    setDyn dyn v1'
    res <- GreaterThanEqual v1 v2 >>= If
    branchMaybe label res

  Op.Insert_obj arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    Objects.insertObj mode v1 v2

  Op.Je args label -> do
    mapM evalArg args >>= EqualAny >>= If >>= branchMaybe label

  Op.Jg arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- GreaterThan v1 v2 >>= If
    branchMaybe label res

  Op.Jin arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    p <- Objects.getFM Parent v1
    res <- EqualAny [v2,p] >>= If
    branchMaybe label res

  Op.Jl arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- LessThan v1 v2 >>= If
    branchMaybe label res

  Op.Jump addr -> do
    pc <- LitA addr
    SetControl AtInstruction { pc }

  Op.Jz arg label -> do evalArg arg >>= IsZero >>= If >>= branchMaybe label

  Op.Load arg target -> do
    dyn <- evalArgAsDyn arg
    v <- evalDyn dyn
    case dyn of
      DSp{} -> Note "TODO: re-push value on stack!" -- TODO: code the correct thing!
      _ -> pure ()
    setTarget target v

  Op.Loadb arg1 arg2 target -> do
    base <- evalArg arg1 >>= Address
    offset <- evalArg arg2
    a <- Offset base offset
    b <- GetByte a
    v <- Widen b
    setTarget target v

  Op.Loadw arg1 arg2 target -> do
    base <- evalArg arg1 >>= Address
    offset <- evalArg arg2
    two <- LitV 2
    doubleOffset <- Mul two offset
    a <- Offset base doubleOffset
    w <- getWord a
    setTarget target w

  Op.Mod arg1 arg2 target -> do evalBin Mod arg1 arg2 target

  Op.Mul arg1 arg2 target -> do evalBin Mul arg1 arg2 target

  Op.New_line -> do LitS "\n" >>= GamePrint

  Op.Or arg1 arg2 target -> do evalBin Or arg1 arg2 target

  Op.Print string -> do LitS string >>= GamePrint

  Op.Print_addr arg -> do
    v <- evalArg arg
    a <- Address v
    s <- GetText a
    GamePrint s

  Op.Print_char arg -> do
    v <- evalArg arg
    s <- SingleChar v
    GamePrint s

  Op.Print_num arg -> do
    v <- evalArg arg
    s <- ShowNumber v
    GamePrint s

  Op.Print_obj arg -> do
    v <- evalArg arg
    shortName <- Objects.getShortName v
    GamePrint shortName

  Op.Print_paddr arg -> do
    v <- evalArg arg
    a <- PackedAddress v
    s <- GetText a
    GamePrint s

  Op.Print_ret string -> do
    s <- LitS (string ++ "\n")
    GamePrint s
    one <- LitV 1
    returnValue one

  Op.Pull arg -> do
    dyn <- evalArgAsDyn arg
    v1 <- PopStack
    setDyn dyn v1

  Op.Push arg -> do evalArg arg >>= PushStack

  Op.Put_prop arg1 arg2 arg3 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    v3 <- evalArg arg3
    Objects.putProp mode v1 v2 v3

  Op.Quit -> do
    Quit

  Op.Random arg target -> do
    v1 <- evalArg arg
    res <- Random v1
    --Debug ("random",v1,"->",res)
    setTarget target res

  Op.Remove_obj arg -> do
    v <- evalArg arg
    Objects.removeObj mode v

  Op.Restart -> do -- TODO: implementation is not stack safe!
    Header{initialPC} <- StoryHeader
    pc <- LitA initialPC
    SetControl AtInstruction { pc }

  Op.Ret_popped -> do PopStack >>= returnValue

  Op.Ret arg -> do
    v <- evalArg arg
    returnValue v

  Op.Rfalse -> do LitV 0 >>= returnValue
  Op.Rtrue -> do LitV 1 >>= returnValue

  Op.Set_attr arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    Objects.setAttr v1 v2

  Op.Sread arg1 arg2 -> do
    Header{zv} <- StoryHeader
    statusInfoM <- if
      | zv <= Z3 -> do
          room <- LitB 0 >>= evalGlobal >>= Objects.getShortName
          score <- LitB 1 >>= evalGlobal
          turns <- LitB 2 >>= evalGlobal
          pure $ Just $ StatusInfo { room, score, turns }
      | otherwise ->
          pure Nothing
    rawTyped <- ReadInputFromUser statusInfoM
    tBuf <- evalArg arg1 >>= Address
    pBuf <- evalArg arg2 >>= Address
    (n,positionedWords,canoicalizedTyped) <- Tokenize rawTyped
    textBytes <- StringBytes canoicalizedTyped
    one <- LitV 1
    tBuf1 <- Offset tBuf one
    ForeachB textBytes $ \off b -> do
      a <- Offset tBuf1 off
      SetByte a b
    pBuf1 <- Offset pBuf one
    SetByte pBuf1 n
    two <- LitV 2
    pBuf2 <- Offset pBuf two
    ForeachBT positionedWords $ \i (pos,word) -> do
      dictAddr <- LookupInDict word
      dictAddrV <- DeAddress dictAddr
      (hi,lo) <- splitWord dictAddrV
      n <- StringLength word
      four <- LitV 4
      off <- Mul four i
      a <- Offset pBuf2 off
      writeBytes a [hi,lo,n,pos]

  Op.Store arg1 arg2 -> do
    dyn <- evalArgAsDyn arg1
    v2 <- evalArg arg2
    case dyn of
      DSp{} -> Error "Op.Store/DSp" -- (do _ <- PopStack; pure ()) -- TODO: (from niz) enable code when hit
      _ -> pure ()
    setDyn dyn v2

  Op.Storeb arg1 arg2 arg3 -> do
    base <- evalArg arg1 >>= Address
    offset <- evalArg arg2
    v3 <- evalArg arg3
    value <- LoByte v3
    a <- Offset base offset
    SetByte a value

  Op.Storew arg1 arg2 arg3 -> do
    base <- evalArg arg1 >>= Address
    offset <- evalArg arg2
    value <- evalArg arg3
    two <-LitV 2
    doubleOffset <- Mul two offset
    a <- Offset base doubleOffset
    setWord a value

  Op.Sub arg1 arg2 target -> do evalBin Sub arg1 arg2 target

  Op.Test arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    v12 <- And v1 v2
    res <- EqualAny [v12, v2] >>= If
    branchMaybe label res

  Op.Test_attr arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- Objects.testAttr v1 v2
    branchMaybe label res

  Op.Read_char target -> do
    Debug("TODO: Read_char",target)
    pure ()

  Op.Buffer_mode{} -> pure ()
  Op.Erase_window{} -> pure ()
  Op.Output_stream1{} -> pure ()
  Op.Output_stream2{} -> pure ()
  Op.Set_cursor{} -> pure ()
  Op.Set_text_style{} -> pure ()
  Op.Set_window{} -> pure ()
  Op.Split_window{} -> pure ()

  Op.Input_stream{} -> undefined

  Op.Nop -> undefined
  Op.Pop -> Note "Pop"
  Op.Restore{} -> Note "Restore" -- TODO: do it properly
  Op.Save{} -> Note "Save" -- TODO: do it properly
  Op.Show_status -> undefined
  Op.Verify{} -> Note "Verify" -- TODO: anything to do here?

writeBytes :: Addr p -> [Byte p] -> Eff p ()
writeBytes a bs =
  sequence_ [ do
                offset <- LitV i
                a' <- Offset a offset
                SetByte a' b
            | (i,b) <- zip [0..] bs
            ]

evalBin :: Phase p => (Value p -> Value p -> Eff p (Value p)) -> Arg -> Arg -> Target -> Eff p ()
evalBin bin arg1 arg2 target = do
  v1 <- evalArg arg1
  v2 <- evalArg arg2
  v <- bin v1 v2
  setTarget target v

branchMaybe :: Phase p => Label -> Bool -> Eff p ()
branchMaybe (Branch sense dest) b = case (sense,b) of
  (Op.T,False) -> pure ()
  (Op.F,True) -> pure ()
  (Op.T,True) -> gotoDest dest
  (Op.F,False) -> gotoDest dest

gotoDest :: Phase p => Dest -> Eff p ()
gotoDest = \case
  Dfalse -> LitV 0 >>= returnValue
  Dtrue -> LitV 1 >>= returnValue
  Dloc addr -> do
    pc <- LitA addr
    SetControl AtInstruction { pc }

evalFunc :: Phase p => Func -> Eff p (Addr p)
evalFunc = \case
  BadFunc -> error "failed to decode called function"
  Floc addr -> LitA addr
  Fvar var -> do
    v <- evalTarget var
    a <- PackedAddress v
    --Debug ("evalFunc/var",a) -- TODO: show dynamically reachable code
    pure a

evalArg :: Phase p => Arg -> Eff p (Value p)
evalArg = \case
  Con x -> LitV x
  Var v -> evalTarget v

evalTarget :: Phase p => Target -> Eff p (Value p)
evalTarget = \case
  Sp -> PopStack
  Local n -> LitB n >>= GetLocal
  Global b -> LitB b >>= evalGlobal

evalGlobal :: Phase p => Byte p -> Eff p (Value p)
evalGlobal b = do
  res <- globalAddr b >>= getWord
  --Debug ("evalGlobal",b,"->",res)
  pure res

returnValue :: Phase p => Value p -> Eff p ()
returnValue result = do
  caller <- PopFrame
  SetControl AtReturnFromCall { caller, result }

setTarget :: Target -> Value p -> Eff p ()
setTarget var v = case var of
  Sp -> PushStack v
  Local n -> LitB n >>= \n -> SetLocal n v
  Global b -> do
    a <- LitB b >>= globalAddr
    setWord a v

globalAddr :: Byte p -> Eff p (Addr p)
globalAddr b = do
  Header{globalVars} <- StoryHeader
  base <- LitA globalVars
  v <- Widen b
  two <- LitV 2
  offset <- Mul two v
  Offset base offset

setActuals :: [Value p] -> Eff p ()
setActuals actuals = do
  indexes <- mapM LitB [1..]
  sequence_ [ SetLocal i def | (i,def) <- zip indexes actuals ]

setDefaults :: RoutineHeader -> Byte p -> Eff p ()
setDefaults rh n =
  case rh of
    Op.BadRoutineHeader -> Error "setDefaults: BadRoutineHeader, n>15"
    Op.RoutineHeader defs -> do
      MakeRoutineFrame (length defs)
      loop (fromIntegral (length defs)) (reverse defs)
        where
          loop i = \case
            [] -> pure ()
            def:defs -> Isolate $ do
              ie <- LitB i
              LessThanByte n ie >>= If >>= \case
                False -> pure ()
                True -> do
                  LitV def >>= SetLocal ie
                  loop (i-1) defs

getWord :: Addr p -> Eff p (Value p)
getWord a = do
  hi <- GetByte a
  one <- LitV 1
  a' <- Offset a one
  lo <- GetByte a'
  MakeHiLo hi lo

setWord :: Addr p -> Value p -> Eff p ()
setWord a w = do
  (hi,lo) <- splitWord w
  SetByte a hi
  one <- LitV 1
  a' <- Offset a one
  SetByte a' lo

splitWord :: Value p -> Eff p (Byte p,Byte p)
splitWord w = do
  hi <- HiByte w
  lo <- LoByte w
  pure (hi,lo)

data Dyn b -- dynamic target
  = DSp
  | DLocal b
  | DGlobal b

evalArgAsDyn :: Phase p => Arg -> Eff p (Dyn (Byte p))
evalArgAsDyn arg = do
  v <- evalArg arg
  lo <- LoByte v
  makeDyn lo

makeDyn :: Byte p -> Eff p (Dyn (Byte p))
makeDyn b = do
  q <- IsZeroByte b >>= If
  case q of
    True -> pure DSp
    False -> do
      sixteen <- LitB 16
      p <- LessThanByte b sixteen >>= If
      if p then pure (DLocal b) else do
        g <- MinusByte b sixteen
        pure (DGlobal g)

evalDyn :: Phase p =>  Dyn (Byte p) -> Eff p (Value p)
evalDyn = \case
  DSp -> PopStack
  DLocal n -> GetLocal n
  DGlobal b -> evalGlobal b

setDyn :: Dyn (Byte p) -> Value p -> Eff p ()
setDyn dyn v = case dyn of
  DSp -> PushStack v
  DLocal n -> SetLocal n v
  DGlobal b -> do
    a <- globalAddr b
    setWord a v
