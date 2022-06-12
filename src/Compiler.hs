
module Compiler (compileEffect,dumpCode,runCode) where

import Action (Action)
import Control.Monad (ap,liftM)
import Data.List (intercalate)
import Decode (fetchOperation,fetchRoutineHeader)
import Disassemble (Routine(..),disRoutine,branchesOf,routinesBetween)
import Eff (Phase,Eff,Control)
import qualified Eff (Eff(..),Control(..))
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Addr,Byte,Value)
import Operation (Operation(Call))
import Story (Story(header,size),OOB_Mode(..))
import Text.Printf (printf)
import qualified Action (Action(..))
import qualified Eff (Phase(..),StatusInfo(..))
import qualified Primitive as Prim

data Compile

instance Phase Compile where
  type Addr Compile = Expression Addr
  type Byte Compile = Expression Byte
  type Pred Compile = Expression Bool
  type Text Compile = Expression String
  type Value Compile = Expression Value
  type Vector Compile a = Expression [a]

type Effect a = Eff Compile a

compileEffect :: Story -> Effect () -> IO Code
compileEffect story smallStep = do
  let endOfStory :: Addr = fromIntegral (size story)
  let Header{initialPC=_, staticMem} = Story.header story
  let as = routinesBetween story (staticMem,endOfStory)
  let routines = [ disRoutine story a | a <- as ]
  Code <$> mapM (compileRoutine story smallStep) routines

compileRoutine :: Story -> Effect () -> Routine -> IO CompiledRoutine
compileRoutine story smallStep routine = do
  let Routine{start,body} = routine
  let
    isCall :: Operation -> Bool
    isCall = \case
      Operation.Call{} -> True
      _ -> False
  let
    locations :: [Loc] =
      [ x
      | (addr,op) <- body
      , x <- [LocOp addr] ++ if isCall op then [LocReturn addr] else []
      ]
  let jumpDest :: [Addr] = concat [branchesOf op | (_,op) <- body ]

  let
    dontInlineSet = case body of
      [] -> jumpDest
      (bodyStart,_):_ -> bodyStart : jumpDest

  let
    addressesToInline :: [Addr] =
      [ a
      | (a,_) <- body
        -- TODO: we could still inline if there is only a single jump-from location & no fallthrough
      , a `notElem` dontInlineSet
      ]
  let
    nonInlinedLocations :: [Loc] =
      [ control
      | control <- locations
      , case control of LocOp a -> a `notElem` addressesToInline; _ -> True
      ]
  let locationsToCompile = [LocRoutine start] ++ nonInlinedLocations
  let
    shouldInline :: Control Compile -> Bool
    shouldInline control =
      case control of
        Eff.AtInstruction (Const control) -> (control `elem` addressesToInline)
        _ -> False

  let
    static = Static { story, smallStep, shouldInline }

  CompiledRoutine <$> mapM (runGen . compileLoc static) locationsToCompile


data Static = Static
  { story :: Story
  , smallStep :: Effect ()
  , shouldInline :: Control Compile -> Bool
  }

compileLoc :: Static -> Loc -> Gen Chunk
compileLoc Static{story,smallStep,shouldInline} loc = do

  let control = makeControl loc
  body <- compileK (initState control) smallStep kJump
  pure Chunk { label = loc, body }

  where
    oob who = OOB_Error ("compileLoc:"++who)

    header@Header{zv} = Story.header story

    -- continuation (for compilation) which will finish the compiled program with a jump
    kJump :: State -> () -> Gen (Prog 'WillJump)
    kJump s () = do
      let State{control} = s
      if
        | shouldInline control -> do
         --Seq (Inlining control) <$>
            compileK s { control } smallStep kJump
        | otherwise -> do
            pure (flushStack s (Jump control))

    -- compile0: compile an effect, in isolation, to a program which will not jump
    compile0 :: State -> Effect () -> Gen (Prog 'WontJump)
    compile0 s eff = do
      compileK s eff $ \s () -> pure (flushStack s Null)

    -- compileK: compile an effect, given a continuation, to a program which may or notjump
    compileK :: forall jumpiness effectType. State -> Effect effectType -> (State -> effectType -> Gen (Prog jumpiness)) -> Gen (Prog jumpiness)
    compileK s e k = case e of
      Eff.Ret x -> k s x
      Eff.Bind e f -> compileK s e $ \s a -> compileK s (f a) k

      Eff.GamePrint mes -> do Seq (GamePrint mes) <$> k s ()

      Eff.Error msg -> do pure $ Error msg
      Eff.Debug msg -> do GDebug msg; k s ()
      Eff.Note x -> Seq (Note (show x)) <$> k s ()

      Eff.StoryHeader -> k s header

      Eff.ReadInputFromUser statusLine -> do
        name <- genId "user_command_line_"
        Seq (ReadInputFromUser statusLine name) <$> k s (Variable name)

      Eff.GetText a -> do
        -- TODO: make special case for constant addresses
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

      Eff.PushFrame addr -> do
        Seq (PushFrame addr) <$> k s ()

      Eff.PopFrame -> do
        name <- genId "return_address_"
        Seq (PopFrame name) <$> k s (Variable name)

      Eff.GetLocal n -> k s (GetLocal n)

      Eff.SetLocal n v -> do
        Seq (SetLocal n v) <$> k s ()

      Eff.GetByte a -> do -- TODO: optimize: lookup in static memory range should happen now
        name <- genId "b"
        Seq (Let (Bind name (GetByte a))) <$> k s (Variable name)

      Eff.SetByte a b ->
        Seq (SetByte a b) <$> k s ()

      Eff.PushStack v -> if
        | eagerStack -> Seq (PushStack v) <$> k s ()
        | otherwise -> do
          let State{stack} = s
          k s { stack = v : stack } ()

      Eff.PopStack -> do
        let State{stack} = s
        case stack of
          v:stack -> do
            k s { stack } v
          [] -> do
            name <- genId "popped"
            Seq (PopStack name) <$> k s (Variable name)

      Eff.Random range -> do
        name <- genId "random"
        Seq (LetRandom name range) <$> k s (Variable name)

      Eff.Quit -> do
        pure Quit

      Eff.If pred -> do
        case pred of
          Const pred -> k s pred
          _ -> do
            b1 <- k s True
            b2 <- k s False
            pure $ If pred b1 b2

      Eff.Isolate eff -> do
        first <- compile0 s eff
        FullSeq first <$> k s ()

      Eff.ForeachB xs f -> do
        index <- genId "index"
        elem <- genId "byte"
        let bodyEff = f (Variable index) (Variable elem)
        body <- compile0 s bodyEff
        ForeachB (index,elem) xs body <$> k s ()

      Eff.ForeachBT xs f -> do
        index <- genId "index"
        elem1 <- genId "pos"
        elem2 <- genId "word"
        let bodyEff = f (Variable index) (Variable elem1,Variable elem2)
        body <- compile0 s bodyEff
        ForeachBT (index,elem1,elem2) xs body <$> k s ()

      Eff.LitA a -> k s (Const a)
      Eff.LitB b -> k s (Const b)
      Eff.LitS x -> k s (Const x)
      Eff.LitV v -> k s (Const v)

      -- pure unary primitives
      Eff.Address x -> prim1 x Prim.Address
      Eff.DeAddress x -> prim1 x Prim.DeAddress
      Eff.Div8 x -> prim1 x Prim.Div8
      Eff.EqualAny x -> k s (Unary Prim.EqualAny (List x))

      Eff.HiByte x -> prim1 x Prim.HiByte
      Eff.IsZero x -> prim1 x Prim.IsZero
      Eff.IsZeroAddress x -> prim1 x Prim.IsZeroAddress
      Eff.IsZeroByte x -> prim1 x Prim.IsZeroByte
      Eff.LoByte x -> prim1 x Prim.LoByte
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
      Eff.GreaterThan x y -> prim2 x y Prim.GreaterThan
      Eff.GreaterThanEqual x y -> prim2 x y Prim.GreaterThanEqual
      Eff.LessThan x y -> prim2 x y Prim.LessThan
      Eff.LessThanByte x y -> prim2 x y Prim.LessThanByte
      Eff.LessThanEqual x y -> prim2 x y Prim.LessThanEqual
      Eff.MakeHiLo x y -> prim2 x y Prim.MakeHiLo
      Eff.MinusByte x y -> prim2 x y Prim.MinusByte
      Eff.Mod x y -> prim2 x y Prim.Mod
      Eff.Mul x y -> prim2 x y Prim.Mul
      Eff.Offset x y -> prim2 x y Prim.Offset
      Eff.Or x y -> prim2 x y Prim.Or
      Eff.SetBit x y -> prim2 x y Prim.SetBit
      Eff.ShiftR x y -> prim2 x (Const y) Prim.ShiftR
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
        b <- genId "position_words_"
        c <- genId "canonicalized"
        Seq (Tokenize x (a,b,c)) <$> k s (Variable a,Variable b,Variable c)

      where
        prim1 :: (Show x) => Expression r ~ effectType => Expression x -> Prim.P1 x r -> Gen (Prog jumpiness)
        prim1 x p1 = k s (makeUnary p1 x)

        prim2 :: (Show x, Show y) => Expression r ~ effectType => Expression x -> Expression y -> Prim.P2 x y r -> Gen (Prog jumpiness)
        prim2 x y p2 = k s (makeBinary p2 x y)


genId :: String -> Gen (Identifier a)
genId tag = do
  u <- GenUnique
  pure $ Identifier tag u


--[state]-------------------------------------------------------------

makeControl :: Loc -> Control Compile
makeControl = \case
  LocRoutine a ->
    Eff.AtRoutineHeader { routine = Const a, numActuals = NumActuals }
  LocOp a ->
    Eff.AtInstruction { pc = Const a }
  LocReturn a ->
    Eff.AtReturnFromCall { caller = Const a, result = CallResult }


data State = State
  { control :: Control Compile
  , stack :: [Expression Value] -- TODO: need notion of sharable?
  }

eagerStack :: Bool
eagerStack = True -- disable the lazy stack optimization

initState :: Control Compile -> State
initState control = State
  { control
  , stack = []
  }

flushStack :: State -> Prog j -> Prog j
flushStack State{stack} s = loop s stack
  where
    loop s = \case
      [] -> s
      x:xs -> loop (Seq (PushStack x) s) xs

--[gen]---------------------------------------------------------------

data Gen a where
  GRet :: a -> Gen a
  GBind :: Gen a -> (a -> Gen b) -> Gen b
  GDebug :: Show x => x -> Gen ()
  GenUnique :: Gen Int

instance Functor Gen where fmap = liftM
instance Applicative Gen where pure = return; (<*>) = ap
instance Monad Gen where return = GRet; (>>=) = GBind

runGen :: Gen a -> IO a
runGen g = fst <$> loop GenState{u=1} g where
  loop :: GenState -> Gen a -> IO (a,GenState)
  loop s = \case
    GRet x -> pure (x,s)
    GBind g f -> do (a,s') <- loop s g; loop s' (f a)
    GDebug msg -> do putStrLn ("**GDebug: " ++ show msg); pure ((),s)
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

--[code]--------------------------------------------------------------

runCode :: Word -> Code -> Action
runCode _ _ = do Action.Debug "**TODO:runCode" $ Action.Stop 99

dumpCode :: Code -> IO ()
dumpCode Code{routines} = do
  printf "Code for %d routines\n" (length routines)
  sequence_
    [ do
        printf "--[%d]--------------------------------------------------\n" i
        mapM_ print chunks
    | (i,CompiledRoutine{chunks}) <- zip [1::Int ..] routines
    ]

data Code = Code
  { routines :: [CompiledRoutine]
  }

data CompiledRoutine = CompiledRoutine
  { chunks :: [Chunk]
  }

data Chunk = Chunk { label :: Loc, body :: Prog 'WillJump }

instance Show Chunk where
  show Chunk{ label, body } =
    intercalate "\n" ((show label ++ ":") : pretty 2 body)

data Loc = LocRoutine Addr | LocOp Addr | LocReturn Addr deriving Show


data Jumpiness = WillJump | WontJump

data Prog :: Jumpiness -> * where
  Null :: Prog 'WontJump
  Quit :: Prog j
  Error :: String -> Prog j
  Jump :: Control Compile -> Prog 'WillJump
  Seq :: Atom ->  Prog j -> Prog j
  FullSeq :: Prog 'WontJump ->  Prog j -> Prog j
  If :: Expression Bool -> Prog j -> Prog j -> Prog j
  ForeachB :: (Identifier Value, Identifier Byte) -> Expression [Expression Byte] -> Prog 'WontJump -> Prog j -> Prog j
  ForeachBT :: (Identifier Value, Identifier Byte, Identifier String) -> Expression [(Expression Byte,Expression String)] -> Prog 'WontJump -> Prog j -> Prog j

pretty :: Int -> Prog j -> [String]
pretty i = \case
  Null -> []
  Quit -> [tab i "Quit"]
  Error msg -> [tab i (show msg)]
  Jump Eff.AtInstruction{pc} ->
    [tab i ("Jump: " ++ show pc)]
  Jump Eff.AtRoutineHeader {routine,numActuals} ->
    [tab i ("JumpCall: " ++ show routine ++ ", #actuals: " ++ show numActuals)]
  Jump Eff.AtReturnFromCall{caller,result} ->
    [tab i ("JumpReturn: " ++ show caller ++ ", result: " ++ show result)]
  Seq a s -> tab i (show a ++ ";") : pretty i s
  FullSeq s1 s2 -> pretty i s1 ++ pretty i s2
  If e s1 Null -> concat
    [ [tab i "if (" ++ show e ++ ") {"]
    , pretty (i+2) s1
    , [tab i "}"]
    ]
  If e Null s2 -> concat
    [ [tab i "if (" ++ show e ++ ") {} else {"]
    , pretty (i+2) s2
    , [tab i "}"]
    ]
  If e s1 s2 -> concat
    [ [tab i "if (" ++ show e ++ ") {"]
    , pretty (i+2) s1
    , [tab i "} else {"]
    , pretty (i+2) s2 ++ [tab i "}"]
    ]
  ForeachB (index,elem) xs body following -> concat
    [ [tab i ("ForeachB: " ++ show (index,elem) ++ " in (" ++ show xs ++ ") {")]
    , pretty (i+2) body
    , [tab i "}"]
    ]
    ++ pretty i following
  ForeachBT (index,elem1,elem2) xs body following -> concat
    [ [tab i ("ForeachB: " ++ show (index,elem1,elem2) ++ " in (" ++ show xs ++ ") {")]
    , pretty (i+2) body
    , [tab i "}"]
    ]
    ++ pretty i following

tab :: Int -> String -> String
tab n s = take n (repeat ' ') ++ s

data Atom
  = SetByte (Expression Addr) (Expression Byte)
  | Note String
  -- | Inlining (Control Compile)
  | TraceOperation (Expression Addr) Operation
  | GamePrint (Expression String)
  | MakeRoutineFrame Int
  | PushFrame (Expression Addr)
  | PopFrame (Identifier Addr)
  | PushStack (Expression Value)
  | PopStack (Identifier Value)
  | SetLocal (Expression Byte) (Expression Value)
  | ReadInputFromUser StatusInfo (Identifier String)
  | StringBytes (Expression String) (Identifier [Expression Byte])
  | Tokenize (Expression String) TokenizeIdents
  | LetRandom (Identifier Value) (Expression Value)
  | Let Bind
  deriving Show

data Bind where
  Bind :: Show x => Identifier x -> Expression x -> Bind

instance Show Bind where
  show = \case
    Bind x e -> show x ++ " = " ++ show e

type StatusInfo = Maybe (Eff.StatusInfo Compile)

type TokenizeIdents =
  ( Identifier Byte
  , Identifier [(Expression Byte,Expression String)]
  , Identifier String
  )

data Expression a where
  Const :: a -> Expression a
  NumActuals :: Expression Byte
  CallResult :: Expression Value
  Variable :: Identifier a -> Expression a
  Unary :: Show x => Prim.P1 x r -> Expression x -> Expression r
  Binary :: (Show x, Show y) => Prim.P2 x y r -> Expression x -> Expression y -> Expression r
  GetByte :: Expression Addr -> Expression Byte
  GetLocal :: Expression Byte -> Expression Value
  GetText :: Expression Addr -> Expression String
  List :: Show x => [Expression x] -> Expression [x]
  LookupInDict :: Expression String -> Expression Addr

instance Show a => Show (Expression a) where
  show = \case
    Const a -> show a
    NumActuals -> "num_actuals"
    CallResult -> "call_result"
    Variable v -> show v
    Unary p1 x -> show p1 ++ "(" ++ show x ++ ")"
    Binary p2 x y -> show p2 ++ "(" ++ show x ++ "," ++ show y ++ ")"
    GetByte a -> "M[" ++ show a ++ "]"
    GetLocal n -> "GetLocal(" ++ show n ++ ")"
    GetText a -> "GetText(" ++ show a ++ ")"
    List xs -> "List(" ++ intercalate "," (map show xs) ++ ")"
    LookupInDict x -> "LookupInDict(" ++ show x ++ ")"

data Identifier a where
  Identifier :: String -> Int -> Identifier a

instance Show (Identifier a) where
  show (Identifier tag i) = tag ++ show i
