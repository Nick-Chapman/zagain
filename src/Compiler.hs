
module Compiler (compileEffect) where

import Action (Conf(..))
import Code (Code(..),CompiledRoutine(..),Chunk(..),Prog(..),Atom(..),Bind(..),Label(..),Loc(..),Expression(..),Identifier(..))
import Control.Monad (when,ap,liftM)
import Data.Set (Set,(\\))
import Decode (fetchOperation,fetchRoutineHeader)
import Disassemble (Routine(..),disRoutine,branchesOf,routinesBetween)
import Eff (Phase,Eff,Control)
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Addr,Byte,Value)
import Operation (Operation(Call,CallN),Func(Fvar))
import Story (Story(header,size),OOB_Mode(..),readStoryByte)
import qualified Data.Set as Set
import qualified Eff (Eff(..),Control(..))
import qualified Eff (Phase(..))
import qualified Primitive as Prim

data DuringCompilation

instance Phase DuringCompilation where
  type Addr DuringCompilation = Expression Addr
  type Byte DuringCompilation = Expression Byte
  type Pred DuringCompilation = Expression Bool
  type Text DuringCompilation = Expression String
  type Value DuringCompilation = Expression Value
  type Vector DuringCompilation a = Expression [a]
  type Code DuringCompilation = (Bind, Label)

type Effect a = Eff DuringCompilation a

compileEffect :: Conf -> Story -> Effect () -> IO Code
compileEffect conf story smallStep = do
  let endOfStory :: Addr = fromIntegral (size story)
  let Header{initialPC=_, staticMem} = Story.header story
  let as = routinesBetween story (staticMem,endOfStory)
  let routines = [ disRoutine story a | a <- as ]

  let addressesToInline = allAddressesToInline routines

  -- Experiment with inlining across routine calls
  let xInline = [] -- [025175]

  let
    shouldInline :: Control DuringCompilation -> Bool
    shouldInline control =
      case control of
        Eff.AtInstruction (Const addr) ->
          addr `Set.member` addressesToInline || addr `elem` xInline
        Eff.AtRoutineHeader{ routine = Const{} } -> True
        Eff.AtReturnFromCall{ caller = Const{} } -> True
        _ -> False

  let static = Static { conf, story, smallStep, shouldInline }

  Code <$> sequence
    [ do compileRoutine static r | r <- routines ]


allAddressesToInline :: [Routine] -> Set Addr
allAddressesToInline routines = do
  let allAddress :: Set Addr = Set.fromList [ a | Routine{body} <- routines, (a,_) <- body ]
  let jumpDest :: [Addr] = concat [ branchesOf op | Routine{body} <- routines, (_,op) <- body ]
  let followIndirectCall :: [Addr] =
        [ a
        | Routine{body} <- routines
        , ((_,op),(a,_)) <- zip body (tail body)
        , case op of
            Call (Fvar{}) _ _ -> True
            CallN (Fvar{}) _ -> True
            _ -> False
        ]
  let routineFirstInstructions :: [Addr] =
        [ a | Routine{body} <- routines, (a,_) <- case body of [] -> []; x:_ -> [x] ]
  let dontInline = Set.fromList (jumpDest ++ followIndirectCall ++ routineFirstInstructions)
  allAddress \\ dontInline


data Static = Static
  { conf :: Conf
  , story :: Story
  , smallStep :: Effect ()
  , shouldInline :: Control DuringCompilation -> Bool
  }


compileRoutine :: Static -> Routine -> IO CompiledRoutine
compileRoutine static routine = do
  let Routine{start,body} = routine
  let Static{conf,shouldInline} = static
  let
    isCall :: Operation -> Bool
    isCall = \case
      Operation.CallN{} -> True
      Operation.Call{} -> True
      _ -> False
  let
    locations =
      [LocRoutine start] ++
      [ loc
      | (addr,op) <- body
      , loc <-
          (let locOp = LocOp addr in
             if shouldInline (makeControl locOp) then [] else [locOp])
          ++
          if isCall op then [LocReturn addr] else []
      ]
  CompiledRoutine <$> sequence
    [ runGen conf (compileLoc static loc) | loc <- locations]


compileLoc :: Static -> Loc Addr -> Gen Chunk
compileLoc Static{story,smallStep,shouldInline} loc = do

  let control = makeControl loc
  body <- compileK (initState control) smallStep kJump
  pure Chunk { label = loc, body }

  where
    oob who = OOB_Error ("compileLoc:"++who)

    header@Header{zv,staticMem} = Story.header story

    isStaticAddress :: Addr -> Bool
    isStaticAddress a = a <= 63 || a > staticMem

    -- continuation (for compilation) which will finish the compiled program with a jump
    kJump :: State -> () -> Gen Prog
    kJump s () = do
      let State{control} = s
      if
        | shouldInline control -> do
            compileK s { control } smallStep kJump
        | otherwise -> do
            pure (flushState s (makeJump control))

    -- compile0: compile an effect, in isolation, to a program which will not jump
    compile0 :: State -> Effect () -> Gen Prog
    compile0 s eff = do
      compileK s eff $ \s () -> pure (flushState s Null)

    -- compileK: compile an effect, given a continuation, to a program which may or notjump
    compileK :: forall effectType. State -> Effect effectType -> (State -> effectType -> Gen Prog) -> Gen Prog
    compileK s e k = case e of
      Eff.Ret x -> k s x -- TODO: avoid need for "Eff." prefix everywhere
      Eff.Bind e f -> compileK s e $ \s a -> compileK s (f a) k

      Eff.GamePrint mes -> do Seq (GamePrint mes) <$> k s ()
      Eff.TextStyle sb -> do
        Seq (Note (show ("TextStyle",sb))) <$> k s ()

      Eff.Error msg -> do pure $ Error msg
      Eff.Debug msg -> do GDebug msg; k s ()
      Eff.Note x -> Seq (Note (show x)) <$> k s ()

      Eff.StoryHeader -> k s header

      Eff.ReadInputFromUser statusLine -> do
        name <- genId "user_command_line_"
        Seq (ReadInputFromUser statusLine name) <$> k s (Variable name)

      Eff.GetText a -> do
        -- TODO: make special case for static addresses.
        k s (GetText a)

      Eff.GetControl -> let State{control} = s in k s control
      Eff.SetControl control -> k s { control } ()

      Eff.FetchOperation pc -> do
        case pc of
          Const pc -> do
            let (ins,pc') = runFetch (oob "Compile/FetchOperation") pc story fetchOperation
            k s (ins, Const pc')
          _ ->
            error "Fetch instruction at non-constant PC"

      Eff.FetchRoutineHeader pc -> do
        case pc of
          Const pc -> do
            let (rh,pc') = runFetch (oob "Compile/FetchRoutineHeader") pc story fetchRoutineHeader
            k s (rh, Const pc')
          _ -> do
            error "Fetch routine header at non-constant PC"

      Eff.TraceOperation addr op -> do
        Seq (TraceOperation addr op) <$> k s ()

      Eff.TraceRoutineCall _addr -> do k s ()

      Eff.MakeRoutineFrame n -> do
        Seq (MakeRoutineFrame n)  <$> k s ()

      Eff.PushFrame addr _numActuals-> do
        Seq PushFrame <$> if
          | eagerStack -> Seq (PushReturnAddress addr) <$> k s ()
          | otherwise -> do
              let State{retStack} = s
              k s { retStack = addr : retStack } ()

      Eff.PopFrame -> do
        Seq PopFrame <$> do
          let State{retStack} = s
          case retStack of
            [] -> do
              name <- genId "return_address_"
              Seq (PopReturnAddress name) <$> k s (Variable name)
            addr:retStack -> do
              k s { retStack } addr

      Eff.GetNumActuals -> k s NumActuals

      Eff.GetLocal n -> k s (GetLocal n)

      Eff.SetLocal n v -> do
        Seq (SetLocal n v) <$> k s ()

      Eff.GetByte a -> do
        case a of
          Const addr
            | isStaticAddress addr -> do
                -- We can't be sure that this is real code we are compiling,
                -- so the address might not be in bounds.
                -- Hence we can't use the OOB_Error mode for reading the story byte.
                let b = readStoryByte OOB_Zero story addr
                k s (Const b)
          _ -> do
            name <- genId "b"
            Seq (Let (Bind name (GetByte a))) <$> k s (Variable name)

      Eff.SetByte a b ->
        Seq (SetByte a b) <$> k s ()

      Eff.PushStack v -> if
        | eagerStack -> Seq (PushStack v) <$> k s ()
        | otherwise -> do
          let State{tmpStack} = s
          k s { tmpStack = v : tmpStack } ()

      Eff.PopStack -> do
        let State{tmpStack} = s
        case tmpStack of
          v:tmpStack -> do
            k s { tmpStack } v
          [] -> do
            name <- genId "popped"
            Seq (PopStack name) <$> k s (Variable name)

      Eff.Random range -> do
        name <- genId "random"
        Seq (LetRandom name range) <$> k s (Variable name)

      Eff.Quit -> do
        pure Quit

      Eff.IteString pred a b -> do
        case pred of
          Const pred -> k s (if pred then a else b)
          _ -> do
            name <- genId "ite_res"
            Seq (Let (Bind name (Ite pred a b))) <$> k s (Variable name)

      Eff.If pred -> do
        case pred of
          Const pred -> k s pred
          _ -> do
            b1 <- k s True
            b2 <- k s False
            pure $ If pred b1 b2

      Eff.Fixpoint init f -> do
        flushStateK s $ \s -> do
          var <- genId "loop_var"
          label <- genLabel
          let tieback exp = do pure (Bind var exp,label)
          let eff :: Effect () = f tieback (Variable var)
          let body :: Gen Prog = compileK s eff k
          Seq (Let (Bind var init)) <$> do
            Labelled label <$> do
              body

      Eff.Link (bind,label) -> do
        Seq (Assign bind) <$> pure (Goto label)

      Eff.Isolate eff -> do
        flushStateK s $ \s -> do
          first <- compile0 s eff
          FullSeq first <$> k s ()

      Eff.IndexVecB vec n -> do
        k s (Join (Binary Prim.IndexList vec n))

      Eff.IndexVecT vec n -> do
        k s (Join (Binary Prim.IndexList vec n))

      Eff.LitA a -> k s (Const a)
      Eff.LitB b -> k s (Const b)
      Eff.LitS x -> k s (Const x)
      Eff.LitV v -> k s (Const v)

      -- pure unary primitives
      Eff.Address x -> prim1 x Prim.Address
      Eff.DeAddress x -> prim1 x Prim.DeAddress
      Eff.Div8 x -> prim1 x Prim.Div8

      Eff.HiByte x -> prim1 x Prim.HiByte
      Eff.IsZero x -> prim1 x Prim.IsZero
      Eff.IsZeroAddress x -> prim1 x Prim.IsZeroAddress
      Eff.IsZeroByte x -> prim1 x Prim.IsZeroByte
      Eff.LoByte x -> prim1 x Prim.LoByte
      Eff.Not x -> prim1 x Prim.Not
      Eff.PackedAddress x -> prim1 x (Prim.PackedAddress zv)
      Eff.SevenMinus x -> prim1 x Prim.SevenMinus
      Eff.ShowNumber x -> prim1 x Prim.ShowNumber
      Eff.SingleChar x -> prim1 x Prim.SingleChar
      Eff.StringLength x -> prim1 x Prim.StringLength
      Eff.Widen x -> prim1 x Prim.Widen

      -- pure binary primitives
      Eff.Add x y -> prim2 x y Prim.Add
      Eff.And x y -> prim2 x y Prim.And
      Eff.BwAnd x y -> prim2 x y Prim.BwAnd
      Eff.ClearBit x y -> prim2 x y Prim.ClearBit
      Eff.Div x y -> prim2 x y Prim.Div
      Eff.Equal x y -> prim2 x y Prim.Equal
      Eff.GreaterThan x y -> prim2 x y Prim.GreaterThan
      Eff.GreaterThanEqual x y -> prim2 x y Prim.GreaterThanEqual
      Eff.LessThan x y -> prim2 x y Prim.LessThan
      Eff.LessThanByte x y -> prim2 x y Prim.LessThanByte
      Eff.LessThanEqual x y -> prim2 x y Prim.LessThanEqual
      Eff.LogOr x y -> prim2 x y Prim.LogOr
      Eff.MakeHiLo x y -> prim2 x y Prim.MakeHiLo
      Eff.MinusByte x y -> prim2 x y Prim.MinusByte
      Eff.Mod x y -> prim2 x y Prim.Mod
      Eff.Mul x y -> prim2 x y Prim.Mul
      Eff.Offset x y -> prim2 x y Prim.Offset
      Eff.Or x y -> prim2 x y Prim.Or
      Eff.SetBit x y -> prim2 x y Prim.SetBit
      Eff.ShiftR x y -> prim2 x y Prim.ShiftR
      Eff.Sub x y -> prim2 x y Prim.Sub
      Eff.TestBit x y -> prim2 x y Prim.TestBit

      Eff.LookupInDict word -> do
        res <- genId "lookee"
        Seq (Let (Bind res (LookupInDict word))) <$> k s (Variable res)

      Eff.StringBytes string -> do
        split <- genId "string_bytes_"
        Seq (StringBytes string split) <$> k s (Variable split)

      Eff.Tokenize x -> do
        a <- genId "num_tokens_"
        b <- genId "positions"
        c <- genId "words"
        d <- genId "canonicalized"
        Seq (Tokenize x (a,b,c,d)) <$> k s (Variable a,Variable b,Variable c,Variable d)

      where
        prim1 :: (Show x) => Expression r ~ effectType => Expression x -> Prim.P1 x r -> Gen Prog
        prim1 x p1 = k s (makeUnary p1 x)

        prim2 :: (Show x, Show y) => Expression r ~ effectType => Expression x -> Expression y -> Prim.P2 x y r -> Gen Prog
        prim2 x y p2 = k s (makeBinary p2 x y)


genId :: String -> Gen (Identifier a)
genId tag = do
  u <- GenUnique
  pure $ Identifier tag u

genLabel :: Gen Label
genLabel = do
  u <- GenUnique
  pure $ Label u


--[state]-------------------------------------------------------------

makeControl :: Loc Addr -> Control DuringCompilation
makeControl = \case
  LocRoutine a ->
    Eff.AtRoutineHeader { routine = Const a, numActuals = NumActuals }
  LocOp a ->
    Eff.AtInstruction { pc = Const a }
  LocReturn a ->
    Eff.AtReturnFromCall { caller = Const a, result = CallResult }

makeJump :: Control DuringCompilation -> Prog
makeJump = \case
  Eff.AtRoutineHeader{routine,numActuals} -> do
    Seq (SetNumberActuals numActuals) $ do
      case routine of
        Const routine -> Jump (LocRoutine routine)
        _ -> JumpIndirect (LocRoutine routine)

  Eff.AtInstruction{pc} -> do
    case pc of
      Const pc -> Jump (LocOp pc)
      _ -> JumpIndirect (LocOp pc)

  Eff.AtReturnFromCall{caller,result} -> do
    Seq (SetResult result) $ do
      case caller of
        Const caller -> Jump (LocReturn caller) -- normally can't occur; but can when inlining
        _ -> JumpIndirect (LocReturn caller)

data State = State
  { control :: Control DuringCompilation
  , tmpStack :: [Expression Value] -- TODO: need notion of sharable?
  , retStack :: [Expression Addr]
  }

eagerStack :: Bool
eagerStack = True -- disable the lazy stack optimization

initState :: Control DuringCompilation -> State
initState control = State
  { control
  , tmpStack = []
  , retStack = []
  }


flushStateK :: State -> (State -> Gen Prog) -> Gen Prog
flushStateK s k =
  flushState s <$> k s { tmpStack = [], retStack = [] }

flushState :: State -> Prog -> Prog
flushState State{tmpStack,retStack} s =
  loopTmpStack (loopRetStack s retStack) tmpStack
  where
    loopTmpStack s = \case
      [] -> s
      x:xs -> loopTmpStack (Seq (PushStack x) s) xs

    loopRetStack s = \case
      [] -> s
      x:xs -> loopRetStack (Seq (PushReturnAddress x) s) xs

--[gen]---------------------------------------------------------------

data Gen a where
  GRet :: a -> Gen a
  GBind :: Gen a -> (a -> Gen b) -> Gen b
  GDebug :: Show x => x -> Gen ()
  GenUnique :: Gen Int

instance Functor Gen where fmap = liftM
instance Applicative Gen where pure = return; (<*>) = ap
instance Monad Gen where return = GRet; (>>=) = GBind

runGen :: Conf -> Gen a -> IO a
runGen Conf{debug} g = fst <$> loop GenState{u=1} g where
  loop :: GenState -> Gen a -> IO (a,GenState)
  loop s = \case
    GRet x -> pure (x,s)
    GBind g f -> do (a,s') <- loop s g; loop s' (f a)
    GDebug msg -> do
      when debug $ putStrLn ("**GDebug: " ++ show msg)
      pure ((),s)
    GenUnique -> let GenState{u} = s in pure (u, GenState {u=u+1})

data GenState = GenState { u :: Int }

--[constant folding]--------------------------------------------------

doConstFolding :: Bool
doConstFolding = True

makeUnary :: Show x => Prim.P1 x r -> Expression x -> Expression r
makeUnary p1 = \case
  Const x | doConstFolding -> Const (Prim.evalP1 p1 x)
  x ->
    case (p1,x) of
      (Prim.LoByte,Binary Prim.MakeHiLo _ lo) -> lo
      (Prim.HiByte,Binary Prim.MakeHiLo hi _) -> hi
      _ -> Unary p1 x

makeBinary :: (Show x, Show y) => Prim.P2 x y r -> Expression x -> Expression y -> Expression r
makeBinary p2 x y = case (x,y) of
  (Const x,Const y) | doConstFolding -> Const (Prim.evalP2 p2 x y)
  _ -> Binary p2 x y
