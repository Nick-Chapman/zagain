
module Compiler (compileEffect) where

import Action (Conf(..))
import Code (Code(..),CompiledRoutine(..),Chunk(..),Prog(Seq),Binding(..),Label(..),Loc(..),Expression(..),Identifier(..))
import Control.Monad (when,ap,liftM)
import Data.Set (Set,(\\))
import Decode (fetchOperation,fetchRoutineHeader,ztext)
import Dictionary (Dict,fetchDict)
import Disassemble (Routine(..),disRoutine,branchesOf,routinesBetween)
import Eff (Phase,Eff(..),Control(..))
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Addr,Byte,Value)
import Operation (Operation(Call,CallN),Func(Fvar))
import Story (Story(header,size),OOB_Mode(..),readStoryByte)
import qualified Code as Atom (Atom(..))
import qualified Code as Prog (Prog(..))
import qualified Data.Set as Set
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
  type Code DuringCompilation = (Binding, Label)

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
        AtInstruction (Const addr) ->
          addr `Set.member` addressesToInline || addr `elem` xInline
        AtRoutineHeader{ routine = Const{} } -> True
        AtReturnFromCall{ caller = Const{} } -> True
        _ -> False

  let (dict,_) = runFetch (OOB_Error "fetchDict") 0 story fetchDict

  let static = Static { conf, story, dict, smallStep, shouldInline }

  compiledRoutines <- sequence [ do compileRoutine static r | r <- routines ]
  pure Code { compiledRoutines, dict }


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
  , dict :: Dict
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
compileLoc Static{story,dict,smallStep,shouldInline} loc = do

  let control = makeControl loc
  body <- compileK (initState control) smallStep kJump
  pure Chunk { label = loc, body }

  where
    oob who = OOB_Error ("compileLoc:"++who)

    header@Header{zv,staticMem} = Story.header story

    isStaticAddress :: Addr -> Bool
    --isStaticAddress a = a <= 63 || a > staticMem
    isStaticAddress a = a > staticMem

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
      compileK s eff $ \s () -> pure (flushState s Prog.Null)

    -- compileK: compile an effect, given a continuation, to a program which may or notjump
    compileK :: forall effectType. State -> Effect effectType -> (State -> effectType -> Gen Prog) -> Gen Prog
    compileK s e k = case e of
      Ret x -> k s x
      Bind e f -> compileK s e $ \s a -> compileK s (f a) k

      GamePrint mes -> do Seq (Atom.GamePrint mes) <$> k s ()
      TextStyle sb -> do
        Seq (Atom.Note (show ("TextStyle",sb))) <$> k s ()

      Error msg -> do pure $ Prog.Error msg
      Debug msg -> do GDebug msg; k s ()
      Note x -> Seq (Atom.Note (show x)) <$> k s ()

      StoryHeader -> k s header

      ReadInputFromUser statusLine -> do
        name <- genId "user_command_line_"
        Seq (Atom.ReadInputFromUser statusLine name) <$> k s (Variable name)

      GetText a -> do
        case a of
          Const a -> do
            let (text,_) = runFetch (oob "Compiler/GetText") a story ztext
            Seq (Atom.Note "<static Gettext address>") <$>
              k s (Const text)
          _ ->
            k s (GetTextE a)

      GetControl -> let State{control} = s in k s control
      SetControl control -> k s { control } ()

      FetchOperation pc -> do
        case pc of
          Const pc -> do
            let (ins,pc') = runFetch (oob "Compile/FetchOperation") pc story fetchOperation
            k s (ins, Const pc')
          _ ->
            error "Fetch instruction at non-constant PC"

      FetchRoutineHeader pc -> do
        case pc of
          Const pc -> do
            let (rh,pc') = runFetch (oob "Compile/FetchRoutineHeader") pc story fetchRoutineHeader
            k s (rh, Const pc')
          _ -> do
            error "Fetch routine header at non-constant PC"

      TraceOperation addr op -> do
        Seq (Atom.TraceOperation addr op) <$> k s ()

      TraceRoutineCall _addr -> do k s ()

      MakeRoutineFrame n -> do
        Seq (Atom.MakeRoutineFrame n)  <$> k s ()

      PushFrame addr _numActuals-> do
        Seq Atom.PushFrame <$> if
          | eagerStack -> Seq (Atom.PushReturnAddress addr) <$> k s ()
          | otherwise -> do
              let State{retStack} = s
              k s { retStack = addr : retStack } ()

      PopFrame -> do
        Seq Atom.PopFrame <$> do
          let State{retStack} = s
          case retStack of
            [] -> do
              name <- genId "return_address_"
              Seq (Atom.PopReturnAddress name) <$> k s (Variable name)
            addr:retStack -> do
              k s { retStack } addr

      GetNumActuals -> k s NumActuals

      GetLocal n -> do
        name <- genId ("local" ++ show n ++ "_")
        Seq (Atom.GetLocal n name) <$> k s (Variable name)

      SetLocal n v -> do
        Seq (Atom.SetLocal n v) <$> k s ()

      GetByte a -> do
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
            Seq (Atom.Let (Binding name (GetByteE a))) <$> k s (Variable name)

      SetByte a b ->
        Seq (Atom.SetByte a b) <$> k s ()

      PushStack v -> if
        | eagerStack -> Seq (Atom.PushStack v) <$> k s ()
        | otherwise -> do
          let State{tmpStack} = s
          k s { tmpStack = v : tmpStack } ()

      PopStack -> do
        let State{tmpStack} = s
        case tmpStack of
          v:tmpStack -> do
            k s { tmpStack } v
          [] -> do
            name <- genId "popped"
            Seq (Atom.PopStack name) <$> k s (Variable name)

      Random range -> do
        name <- genId "random"
        Seq (Atom.LetRandom name range) <$> k s (Variable name)

      Quit -> do
        pure Prog.Quit

      IteString pred a b -> do
        case pred of
          Const pred -> k s (if pred then a else b)
          _ -> do
            name <- genId "ite_res"
            Seq (Atom.Let (Binding name (Ite pred a b))) <$> k s (Variable name)

      If pred -> do
        case pred of
          Const pred -> k s pred
          _ -> do
            b1 <- k s True
            b2 <- k s False
            pure $ Prog.If pred b1 b2

      Fixpoint init f -> do
        flushStateK s $ \s -> do
          var <- genId "loop_var"
          label <- genLabel
          let tieback exp = do pure (Binding var exp,label)
          let eff :: Effect () = f tieback (Variable var)
          let body :: Gen Prog = compileK s eff k
          Seq (Atom.Let (Binding var init)) <$> do
            Prog.Labelled label <$> do
              body

      Link (bind,label) -> do
        Seq (Atom.Assign bind) <$> pure (Prog.Goto label)

      Isolate eff -> do
        flushStateK s $ \s -> do
          first <- compile0 s eff
          Prog.FullSeq first <$> k s ()

      IndexVecB vec n -> do
        k s (Join (Binary Prim.IndexList vec n))

      IndexVecT vec n -> do
        k s (Join (Binary Prim.IndexList vec n))

      LitA a -> k s (Const a)
      LitB b -> k s (Const b)
      LitS x -> k s (Const x)
      LitV v -> k s (Const v)

      -- pure unary primitives
      Address x -> prim1 x Prim.Address
      DeAddress x -> prim1 x Prim.DeAddress
      Div8 x -> prim1 x Prim.Div8

      HiByte x -> prim1 x Prim.HiByte
      IsZero x -> prim1 x Prim.IsZero
      IsZeroAddress x -> prim1 x Prim.IsZeroAddress
      IsZeroByte x -> prim1 x Prim.IsZeroByte
      LoByte x -> prim1 x Prim.LoByte
      Not x -> prim1 x Prim.Not
      PackedAddress x -> prim1 x (Prim.PackedAddress zv)
      SevenMinus x -> prim1 x Prim.SevenMinus
      ShowNumber x -> prim1 x Prim.ShowNumber
      SingleChar x -> prim1 x Prim.SingleChar
      StringLength x -> prim1 x Prim.StringLength
      Widen x -> prim1 x Prim.Widen

      -- pure binary primitives
      Add x y -> prim2 x y Prim.Add
      And x y -> prim2 x y Prim.And
      BwAnd x y -> prim2 x y Prim.BwAnd
      ClearBit x y -> prim2 x y Prim.ClearBit
      Div x y -> prim2 x y Prim.Div
      Equal x y -> prim2 x y Prim.Equal
      GreaterThan x y -> prim2 x y Prim.GreaterThan
      GreaterThanEqual x y -> prim2 x y Prim.GreaterThanEqual
      LessThan x y -> prim2 x y Prim.LessThan
      LessThanByte x y -> prim2 x y Prim.LessThanByte
      LessThanEqual x y -> prim2 x y Prim.LessThanEqual
      LogOr x y -> prim2 x y Prim.LogOr
      MakeHiLo x y -> prim2 x y Prim.MakeHiLo
      MinusByte x y -> prim2 x y Prim.MinusByte
      Mod x y -> prim2 x y Prim.Mod
      Mul x y -> prim2 x y Prim.Mul
      Offset x y -> prim2 x y Prim.Offset
      Or x y -> prim2 x y Prim.Or
      SetBit x y -> prim2 x y Prim.SetBit
      ShiftR x y -> prim2 x y Prim.ShiftR
      Sub x y -> prim2 x y Prim.Sub
      TestBit x y -> prim2 x y Prim.TestBit

      LookupInDict word -> do
        res <- genId "lookee"
        Seq (Atom.Let (Binding res (LookupInDictE word))) <$> k s (Variable res) -- TODO: prefer unary op

      StringBytes string -> do
        split <- genId "string_bytes_"
        Seq (Atom.StringBytes string split) <$> k s (Variable split)

      Tokenize x -> do
        a <- genId "num_tokens_"
        b <- genId "positions"
        c <- genId "words"
        Seq (Atom.Tokenize x (a,b,c)) <$> k s (Variable a,Variable b,Variable c)

      where
        prim1 :: (Show x) => Expression r ~ effectType => Expression x -> Prim.P1 x r -> Gen Prog
        prim1 x p1 = k s (makeUnary dict p1 x)

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
    AtRoutineHeader { routine = Const a, numActuals = NumActuals }
  LocOp a ->
    AtInstruction { pc = Const a }
  LocReturn a ->
    AtReturnFromCall { caller = Const a, result = CallResult }

makeJump :: Control DuringCompilation -> Prog
makeJump = \case
  AtRoutineHeader{routine,numActuals} -> do
    Seq (Atom.SetNumberActuals numActuals) $ do
      case routine of
        Const routine -> Prog.Jump (LocRoutine routine)
        _ -> Prog.JumpIndirect (LocRoutine routine)

  AtInstruction{pc} -> do
    case pc of
      Const pc -> Prog.Jump (LocOp pc)
      _ -> Prog.JumpIndirect (LocOp pc)

  AtReturnFromCall{caller,result} -> do
    Seq (Atom.SetResult result) $ do
      case caller of
        Const caller -> Prog.Jump (LocReturn caller) -- normally can't occur; but can when inlining
        _ -> Prog.JumpIndirect (LocReturn caller)

data State = State
  { control :: Control DuringCompilation
  , tmpStack :: [Expression Value] -- TODO: need notion of sharable?
  , retStack :: [Expression Addr]
  }

eagerStack :: Bool
eagerStack = True -- disable the lazy stack optimization -- TODO: enable/explore/test

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
      x:xs -> loopTmpStack (Seq (Atom.PushStack x) s) xs

    loopRetStack s = \case
      [] -> s
      x:xs -> loopRetStack (Seq (Atom.PushReturnAddress x) s) xs

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

makeUnary :: Show x => Dict -> Prim.P1 x r -> Expression x -> Expression r
makeUnary dict p1 = \case
  Const x | doConstFolding -> Const (Prim.evalP1 dict p1 x)
  x ->
    case (p1,x) of
      (Prim.LoByte,Binary Prim.MakeHiLo _ lo) -> lo
      (Prim.HiByte,Binary Prim.MakeHiLo hi _) -> hi
      _ -> Unary p1 x

makeBinary :: (Show x, Show y) => Prim.P2 x y r -> Expression x -> Expression y -> Expression r
makeBinary p2 x y = case (x,y) of
  (Const x,Const y) | doConstFolding -> Const (Prim.evalP2 p2 x y)
  _ -> Binary p2 x y
