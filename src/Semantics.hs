
-- | Semantics of z-machine operations.
module Semantics (theEffect) where

import Dictionary (Dict(..))
import Eff (Eff(..),Bin(..))
import Header (Header(..))
import Objects (FamilyMember(Parent,Sibling,Child))
import Operation (Operation,RoutineHeader,Func(..),Arg(..),Target(..),Label(..),Dest(..))
import Text.Printf (printf)
import qualified Objects
import qualified Operation as Op

theEffect :: Show a => Eff a b s v ()
theEffect = loop
  where
    loop = do
      pc <- GetPC -- only in case we fail to decode
      i <- FetchI
      eval pc i
      loop

eval :: Show a => a -> Operation -> Eff a b s v ()
eval pc = \case

  Op.BadOperation mes -> do
    error (printf "At [%s] %s" (show pc) mes)

  Op.Add arg1 arg2 target -> do evalBin BAdd arg1 arg2 target
  Op.And arg1 arg2 target -> do evalBin BAnd arg1 arg2 target

  Op.Call func args target -> do
    funcAddress <- evalFunc func
    p <- IsZeroAddress funcAddress
    if p then LitV 0 >>= setTarget target else do
      actuals <- mapM evalArg args
      PushFrame funcAddress target
      rh <- FetchRoutineHeader
      setLocals rh actuals

  Op.Clear_attr arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    Objects.clearAttr v1 v2

  Op.Dec arg -> do
    dyn <- evalArgAsDyn arg
    v <- evalDyn dyn
    one <- LitV 1
    v' <- BinOp BSub v one
    setDyn dyn v'

  Op.Dec_chk arg1 arg2 label -> do
    dyn <- evalArgAsDyn arg1
    v1 <- evalDyn dyn
    v2 <- evalArg arg2
    res <- LessThanEqual v1 v2
    one <- LitV 1
    v1' <- BinOp BSub v1 one
    setDyn dyn v1'
    branchMaybe label res

  Op.Div arg1 arg2 target -> do evalBin BDiv arg1 arg2 target

  Op.Get_child arg target label -> do
    v <- evalArg arg
    res <- Objects.getFM Child v
    setTarget target res
    b <- IsZero res
    branchMaybe label (not b)

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
    b <- IsZero res
    branchMaybe label (not b)

  Op.Inc arg -> do
    dyn <- evalArgAsDyn arg
    v <- evalDyn dyn
    one <- LitV 1
    v' <- BinOp BAdd v one
    setDyn dyn v'

  Op.Inc_chk arg1 arg2 label -> do
    dyn <- evalArgAsDyn arg1
    v1 <- evalDyn dyn
    v2 <- evalArg arg2
    one <- LitV 1
    v1' <- BinOp BAdd v1 one
    setDyn dyn v1'
    res <- GreaterThanEqual v1 v2
    branchMaybe label res

  Op.Insert_obj arg1 arg2 -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    Objects.insertObj v1 v2

  Op.Je args label -> do
    mapM evalArg args >>= EqualAny >>= branchMaybe label

  Op.Jg arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- GreaterThan v1 v2
    branchMaybe label res

  Op.Jin arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    p <- Objects.getFM Parent v1
    res <- EqualAny [v2,p]
    branchMaybe label res

  Op.Jl arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    res <- LessThan v1 v2
    branchMaybe label res

  Op.Jump addr -> do LitA addr >>= SetPC
  Op.Jz arg label -> do evalArg arg >>= IsZero >>= branchMaybe label

  Op.Load arg target -> do
    dyn <- evalArgAsDyn arg
    v <- evalDyn dyn
    case dyn of
      DSp{} -> error "TODO: re-push value on stack!"
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
    doubleOffset <- BinOp BMul two offset
    a <- Offset base doubleOffset
    w <- getWord a
    setTarget target w

  Op.Mul arg1 arg2 target -> do evalBin BMul arg1 arg2 target

  Op.New_line -> do LitS "\n" >>= GamePrint
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
    Objects.putProp v1 v2 v3

  Op.Quit -> do
    Quit

  Op.Random arg target -> do
    v1 <- evalArg arg
    res <- Random v1
    --Debug ("random",v1,"->",res)
    setTarget target res

  Op.Remove_obj arg -> do
    v <- evalArg arg
    Objects.removeObj v

  Op.Restart -> do -- TODO: implementation is not stack safe!
    Header{initialPC} <- StoryHeader
    LitA initialPC >>= SetPC

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
    zero <- LitB 0
    one <- LitB 0
    two <- LitB 0
    v0 <- evalGlobal zero
    score <- evalGlobal one
    turns <- evalGlobal two
    p1 <- Objects.getShortName v0
    rawTyped <- ReadInputFromUser (p1,score,turns)
    t_buf <- evalArg arg1 >>= Address
    p_buf <- evalArg arg2 >>= Address
    Dict{seps,entryLength,strings=constStrings} <- FetchDict
    strings <- mapM LitS constStrings
    -- +4 : #seps byte, entryLength byte, #entries word
    Header{dictionary} <- StoryHeader
    offset <- LitV (fromIntegral $ length seps + 4)

    base <- LitA dictionary
    baseEntries <- Offset base offset
    (positionedWords,canoicalizedTyped) <- Tokenize rawTyped

    one <- LitV 1
    a <- Offset t_buf one
    StringBytes canoicalizedTyped >>= writeBytes a

    -- TODO: Process the VEC some kind of `ForEach' effect here?
    quads <- sequence
      [
        do
          iopt <- LookupInStrings strings word
          dictAddr <-
            case iopt of
              Just i -> do
                let ii :: Int = (i-1) * entryLength
                offset <- LitV (fromIntegral ii)
                Offset baseEntries offset
              Nothing -> LitA 0
          dictAddrV <- DeAddress dictAddr
          (hi,lo) <- splitWord dictAddrV
          n <- StringLength word
          pure [ hi, lo, n, pos ]

      | (pos,word) <- positionedWords
      ]
    n <- ListLength quads
    let bs = n : concat quads
    one <- LitV 1
    a <- Offset p_buf one
    writeBytes a bs

  Op.Store arg1 arg2 -> do
    dyn <- evalArgAsDyn arg1
    v2 <- evalArg arg2
    case dyn of
      DSp{} -> undefined (do _ <- PopStack; pure ()) -- TODO: (from niz) enable code when hit
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
    doubleOffset <- BinOp BMul two offset
    a <- Offset base doubleOffset
    setWord a value

  Op.Sub arg1 arg2 target -> do evalBin BSub arg1 arg2 target

  Op.Test arg1 arg2 label -> do
    v1 <- evalArg arg1
    v2 <- evalArg arg2
    v12 <- BwAndV v1 v2
    res <- EqualAny [v12, v2]
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


writeBytes :: a -> [b] -> Eff a b s v ()
writeBytes a bs =
  sequence_ [ do
                offset <- LitV i
                a' <- Offset a offset
                SetByte a' b
            | (i,b) <- zip [0..] bs
            ]

evalBin :: Bin -> Arg -> Arg -> Target -> Eff a b s v ()
evalBin bin arg1 arg2 target = do
  v1 <- evalArg arg1
  v2 <- evalArg arg2
  v <- BinOp bin v1 v2
  setTarget target v

branchMaybe :: Label -> Bool -> Eff a b s v ()
branchMaybe (Branch sense dest) b = case (sense,b) of
  (Op.T,False) -> pure ()
  (Op.F,True) -> pure ()
  (Op.T,True) -> gotoDest dest
  (Op.F,False) -> gotoDest dest

gotoDest :: Dest -> Eff a b s v ()
gotoDest = \case
  Dfalse -> LitV 0 >>= returnValue
  Dtrue -> LitV 1 >>= returnValue
  Dloc addr -> LitA addr >>= SetPC

evalFunc :: Func -> Eff a b s v a
evalFunc = \case
  BadFunc -> error "failed to decode called function"
  Floc addr -> LitA addr
  Fvar var -> do
    v <- evalTarget var
    a <- PackedAddress v
    --Debug ("evalFunc/var",a) -- TODO: show dynamically reachable code
    pure a

evalArg :: Arg -> Eff a b s v v
evalArg = \case
  Con x -> LitV x
  Var v -> evalTarget v

evalTarget :: Target -> Eff a b s v v
evalTarget = \case
  Sp -> PopStack
  Local n -> LitB n >>= GetLocal
  Global b -> LitB b >>= evalGlobal

evalGlobal :: b -> Eff a b s v v
evalGlobal b = globalAddr b >>= getWord

returnValue :: v -> Eff a b s v ()
returnValue v = do
  target <- PopFrame
  setTarget target v

setTarget :: Target -> v -> Eff a b s v ()
setTarget var v = case var of
  Sp -> PushStack v
  Local n -> LitB n >>= \n -> SetLocal n v
  Global b -> do
    a <- LitB b >>= globalAddr
    setWord a v

globalAddr :: b -> Eff a b s v a
globalAddr b = do
  Header{globalVars} <- StoryHeader
  base <- LitA globalVars
  v <- Widen b
  two <- LitV 2
  offset <- BinOp BMul two v
  Offset base offset

setLocals :: RoutineHeader -> [v] -> Eff a b s v ()
setLocals rh actuals =
  case rh of
    Op.BadRoutineHeader -> error "setLocals: BadRoutineHeader, n>15"
    Op.RoutineHeader defs -> do
      -- TODO: we can do better here... !
      indexes <- mapM LitB [1..]
      defs <- mapM LitV defs
      sequence_ [ SetLocal n v | (n,v) <- zip indexes defs ]
      sequence_ [ SetLocal n v | (n,v) <- zip indexes actuals ]

getWord :: a -> Eff a b s v v
getWord a = do
  hi <- GetByte a
  one <- LitV 1
  a' <- Offset a one
  lo <- GetByte a'
  MakeWord hi lo

setWord :: a -> v -> Eff a b s v ()
setWord a w = do
  (hi,lo) <- splitWord w
  SetByte a hi
  one <- LitV 1
  a' <- Offset a one
  SetByte a' lo

splitWord :: v -> Eff a b s v (b,b)
splitWord w = do
  hi <- HiByte w
  lo <- LoByte w
  pure (hi,lo)

data Dyn b -- dynamic target
  = DSp
  | DLocal b
  | DGlobal b

evalArgAsDyn :: Arg -> Eff a b s v (Dyn b)
evalArgAsDyn arg = do
  v <- evalArg arg
  lo <- LoByte v
  makeDyn lo

makeDyn :: b -> Eff a b s v (Dyn b)
makeDyn b = do
  q <- IsZeroByte b
  case q of
    True -> pure DSp
    False -> do
      sixteen <- LitB 16
      p <- LessThanByte b sixteen
      if p then pure (DLocal b) else do
        g <- MinusByte b sixteen
        pure (DGlobal g)

evalDyn :: Dyn b -> Eff a b s v v
evalDyn = \case
  DSp -> PopStack
  DLocal n -> GetLocal n
  DGlobal b -> evalGlobal b

setDyn :: Dyn b -> v -> Eff a b s v ()
setDyn dyn v = case dyn of
  DSp -> PushStack v
  DLocal n -> SetLocal n v
  DGlobal b -> do
    a <- globalAddr b
    setWord a v

