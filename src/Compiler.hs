
module Compiler (compileEffect,runCode) where

import Action (Action)
import Control.Monad (ap,liftM)
import Data.List (intercalate)
import Decode (fetchOperation,fetchRoutineHeader)
import Disassemble (Routine(..),disRoutine,branchesOf)
import Eff (Eff(..),Phase,PC(..))
import Fetch (runFetch)
import Header (Header(..))
import Numbers (Addr,Byte,Value)
import Operation (Operation(Call))
import Story (Story(header),OOB_Mode(..))
import Text.Printf (printf)
import qualified Action (Action(..))
import qualified Eff (Phase(..))
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

runCode :: Word -> Code -> Action
runCode _ _ = do Action.Debug "**TODO:runCode" $ Action.Stop 99

compileEffect :: Story -> Effect () -> IO Code
compileEffect story smallStep = do
  let Header{initialPC} = Story.header story

  let firstRoutine = initialPC - 1
  let Routine{body=routineBody} = disRoutine story firstRoutine

  let
    mkRoutinePC a = do
      AtRoutineHeader { routine = Const a, numActuals = NumActuals }

  let
    mkReturnPC a = do
      AtReturnFromCall { caller = Const a, result = CallResult }

  let
    isCall :: Operation -> Bool
    isCall = \case
      Operation.Call{} -> True
      _ -> False

  let
    locations :: [PC Compile] =
      [ x
      | (addr,op) <- routineBody
      , x <- [AtInstruction (Const addr)] ++ if isCall op then [mkReturnPC addr] else []
      ]

  let routine0 = mkRoutinePC firstRoutine

  let jumpDest :: [Addr] = concat [branchesOf op | (_,op) <- routineBody ]

  let
    addressesToInline :: [Addr] =
      [ a
      | (a,_) <- routineBody
        -- TODO: we could still inline if there is only a single jump-from location & no fallthrough
      , a `notElem` jumpDest
      ]

  let
    nonInlinedLocations :: [PC Compile] =
      [ pc
      | pc <- locations
      , case pc of AtInstruction (Const a) -> a `notElem` addressesToInline; _ -> True
      ]

  let toCompile :: [PC Compile] = [routine0] ++ nonInlinedLocations

  chunks <- sequence
    [ do
        statement <- runGen $ compileFrom pc addressesToInline
        printf "--[%d]--------------------------------------------------\n" i
        print statement
        pure statement
    | (i,pc) <- zip [1::Int ..] toCompile
    ]
  pure $ Code { chunks }

  -- TODO: un-nest this code for better clarity!
  where
    oob who = OOB_Error ("compileEffect:"++who)

    header@Header{zv} = Story.header story

    compileFrom :: PC Compile -> [Addr] -> Gen Statement
    compileFrom pc addressesToInline = do
      S_Label pc <$> continue
      where

        continue :: Gen Statement
        continue = loop (initState pc) smallStep finish

        doInline :: PC Compile -> Bool
        doInline pc =
          case pc of
            AtInstruction (Const pc) -> (pc `elem` addressesToInline)
            _ -> False

        finish :: State -> () -> Gen Statement
        finish s () = do
          let State{pc} = s
          if
            | doInline pc -> do
                -- TODO: comment in gen code that we are inlining
                loop s (do SetPC pc; smallStep) finish
            | otherwise -> do
                pure (S_Done (DoneJump pc))

    loop :: forall loopType. State -> Effect loopType -> (State -> loopType -> Gen Statement) -> Gen Statement
    loop s e k = case e of
      Ret x -> k s x
      Bind e f -> loop s e $ \s a -> loop s (f a) k

      GamePrint mes -> do S_Seq (AtomGamePrint mes) <$> k s ()
      Debug thing -> do GDebug thing; k s ()

      Error msg -> do
        pure $ S_Done (DoneError msg)

      TheDictionary -> undefined
      StoryHeader -> k s header
      ReadInputFromUser _ -> undefined
      GetText a -> undefined a

      GetPC -> let State{pc} = s in k s pc
      SetPC pc -> k s { pc } ()

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

      TraceOperation a op -> do
        S_Seq (AtomTraceOperation a op) <$> k s ()

      TraceRoutineCall addr -> do
        S_Seq (AtomTraceRoutineCall addr) <$> k s ()

      PushFrame -> do
        S_Seq AtomPushFrame  <$> k s ()

      PopFrame -> do
        undefined

      PushCallStack pc -> do
        S_Seq (AtomPushReturnAddress pc) <$> k s ()

      PopCallStack -> do
        undefined

      GetLocal n -> do
        undefined n

      SetLocal n v -> do
        S_Seq (AtomSetLocal n v) <$> k s ()

      GetByte a -> k s (E_GetByte a)
      SetByte a b -> S_Seq (AtomSetByte a b) <$> k s ()

      -- TODO: explore maintaining temporary-stack at compile-time
      PushStack v -> do
        S_Seq (AtomPushStack v) <$> k s ()

      PopStack -> do
        u <- GenUnique
        let name :: Identifier Value = Identifier u
        let v :: Expression Value = Variable name
        S_Seq (AtomPopStack name) <$> k s v

      Random range -> do
        undefined range

      Quit -> do
        undefined

      If pred -> do
        case pred of
          Const pred -> k s pred
          _ -> do
            b1 <- k s True
            b2 <- k s False
            pure $ S_If pred b1 b2

      Foreach xs f -> do
        undefined xs f

      LitA a -> k s (Const a)
      LitB b -> k s (Const b)
      LitS x -> k s (Const x)
      LitV v -> k s (Const v)

      -- pure unary primitives
      Address x -> prim1 x Prim.Address
      DeAddress x -> prim1 x Prim.DeAddress
      Div8 x -> prim1 x Prim.Div8
      EqualAny x -> k s (Unary Prim.EqualAny (E_List x))

      HiByte x -> prim1 x Prim.HiByte
      IsZero x -> prim1 x Prim.IsZero
      IsZeroAddress x -> prim1 x Prim.IsZeroAddress
      IsZeroByte x -> prim1 x Prim.IsZeroByte
      LoByte x -> prim1 x Prim.LoByte
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
      GreaterThan x y -> prim2 x y Prim.GreaterThan
      GreaterThanEqual x y -> prim2 x y Prim.GreaterThanEqual
      LessThan x y -> prim2 x y Prim.LessThan
      LessThanByte x y -> prim2 x y Prim.LessThanByte
      LessThanEqual x y -> prim2 x y Prim.LessThanEqual
      MakeHiLo x y -> prim2 x y Prim.MakeHiLo
      MinusByte x y -> prim2 x y Prim.MinusByte
      Mod x y -> prim2 x y Prim.Mod
      Mul x y -> prim2 x y Prim.Mul
      Offset x y -> prim2 x y Prim.Offset
      Or x y -> prim2 x y Prim.Or
      SetBit x y -> prim2 x y Prim.SetBit
      ShiftR x y -> prim2 x (Const y) Prim.ShiftR -- TODO: avoid special case
      Sub x y -> prim2 x y Prim.Sub
      TestBit x y -> prim2 x y Prim.TestBit

      LookupInStrings{} -> undefined
      StringBytes{} -> undefined
      Tokenize{} -> undefined

      where
        prim1 :: (Show x) => Expression r ~ loopType => Expression x -> Prim.P1 x r -> Gen Statement
        prim1 x p1 = k s (makeUnary p1 x)

        prim2 :: (Show x, Show y) => Expression r ~ loopType => Expression x -> Expression y -> Prim.P2 x y r -> Gen Statement
        prim2 x y p2 = k s (makeBinary p2 x y)


--[state]-------------------------------------------------------------

data State = State
  { pc :: PC Compile
  }

initState :: PC Compile -> State
initState pc = State
  { pc
  }

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
  x -> Unary p1 x

makeBinary :: (Show x, Show y) => Prim.P2 x y r -> Expression x -> Expression y -> Expression r
makeBinary p2 x y = case (x,y) of
  (Const x,Const y) | doConstFolding -> Const (Prim.evalP2 p2 x y)
  _ -> Binary p2 x y

--[code]--------------------------------------------------------------

data Code = Code
  { chunks :: [Statement]
  }

data Statement
  = S_Done Done
  | S_Seq Atom Statement
  | S_If (Expression Bool) Statement Statement
  | S_Label (PC Compile) Statement

instance Show Statement where show = intercalate "\n" . pretty 0

pretty :: Int -> Statement -> [String]
pretty i = \case
  S_Done x -> [tab i (show x)]
  S_Seq a s -> tab i (show a ++ ";") : pretty i s
  S_If e s1 s2 -> concat
    [ [tab i "if (" ++ show e ++ ") {"]
    , pretty (i+2) s1
    , [tab i "} else {"]
    , pretty (i+2) s2 ++ [tab i "}"]
    ]
  S_Label a s -> concat
    [ [tab i (show a ++ ":")]
    , pretty (i+2) s
    ]

tab :: Int -> String -> String
tab n s = take n (repeat ' ') ++ s

data Done
  = DoneError String
  | DoneJump (PC Compile)
  deriving Show

data Atom
  = AtomSetByte (Expression Addr) (Expression Byte)
  | AtomGamePrint (Expression String)
  | AtomTraceOperation (Expression Addr) Operation
  | AtomTraceRoutineCall (Expression Addr)
  | AtomPushFrame
  | AtomPushReturnAddress (Expression Addr) -- TODO: needs to be an expression?
  | AtomPushStack (Expression Value)
  | AtomPopStack (Identifier Value)
  | AtomSetLocal (Expression Byte) (Expression Value)
  deriving Show

data Expression a where
  Const :: a -> Expression a
  NumActuals :: Expression Byte
  CallResult :: Expression Value
  Variable :: Identifier a -> Expression a
  Unary :: Show x => Prim.P1 x r -> Expression x -> Expression r
  Binary :: (Show x, Show y) => Prim.P2 x y r -> Expression x -> Expression y -> Expression r
  E_GetByte :: Expression Addr -> Expression Byte
  E_List :: Show x => [Expression x] -> Expression [x]

instance Show a => Show (Expression a) where
  show = \case
    Const a -> show a
    NumActuals -> "num_actuals"
    CallResult -> "call_result"
    Variable v -> show v
    Unary p1 x -> show p1 ++ "(" ++ show x ++ ")"
    Binary p2 x y -> show p2 ++ "(" ++ show x ++ "," ++ show y ++ ")"
    E_GetByte a -> "M[" ++ show a ++ "]"
    E_List xs -> "List(" ++ intercalate "," (map show xs) ++ ")"

data Identifier a where
  Identifier :: Int -> Identifier a

instance Show (Identifier a) where
  show (Identifier i) = "v_" ++ show i
