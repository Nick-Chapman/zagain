
module Compiler (compileEffect,runCode) where

import Action (Action)
import Control.Monad (ap,liftM)
import Data.List (intercalate)
import Decode (fetchOperation,fetchRoutineHeader)
import Disassemble (Routine(..),disRoutine,branchesOf)
import Eff (Phase,Eff,PC)
import qualified Eff (Eff(..),PC(..))
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
      Eff.AtRoutineHeader { routine = Const a, numActuals = NumActuals }

  let
    mkReturnPC a = do
      Eff.AtReturnFromCall { caller = Const a, result = CallResult }

  let
    isCall :: Operation -> Bool
    isCall = \case
      Operation.Call{} -> True
      _ -> False

  let
    locations :: [PC Compile] =
      [ x
      | (addr,op) <- routineBody
      , x <- [Eff.AtInstruction (Const addr)] ++ if isCall op then [mkReturnPC addr] else []
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
      , case pc of Eff.AtInstruction (Const a) -> a `notElem` addressesToInline; _ -> True
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
            Eff.AtInstruction (Const pc) -> (pc `elem` addressesToInline)
            _ -> False

        finish :: State -> () -> Gen Statement
        finish s () = do
          let State{pc} = s
          if
            | doInline pc -> do
                -- TODO: comment in gen code that we are inlining
                loop s (do Eff.SetPC pc; smallStep) finish -- TODO: dont need to manufacture effect here
            | otherwise -> do
                pure (S_Done (DoneJump pc))

    loop :: forall loopType. State -> Effect loopType -> (State -> loopType -> Gen Statement) -> Gen Statement
    loop s e k = case e of
      Eff.Ret x -> k s x
      Eff.Bind e f -> loop s e $ \s a -> loop s (f a) k

      Eff.GamePrint mes -> do S_Seq (GamePrint mes) <$> k s ()
      Eff.Debug thing -> do GDebug thing; k s ()

      Eff.Error msg -> do
        pure $ S_Done (DoneError msg)

      Eff.TheDictionary -> undefined
      Eff.StoryHeader -> k s header
      Eff.ReadInputFromUser _ -> undefined
      Eff.GetText a -> undefined a

      Eff.GetPC -> let State{pc} = s in k s pc
      Eff.SetPC pc -> k s { pc } ()

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

      Eff.TraceOperation a op -> do
        S_Seq (TraceOperation a op) <$> k s ()

      Eff.TraceRoutineCall addr -> do
        S_Seq (TraceRoutineCall addr) <$> k s ()

      Eff.PushFrame -> do
        S_Seq PushFrame  <$> k s ()

      Eff.PopFrame -> do
        undefined

      Eff.PushCallStack pc -> do
        S_Seq (PushReturnAddress pc) <$> k s ()

      Eff.PopCallStack -> do
        undefined

      Eff.GetLocal n -> do
        undefined n

      Eff.SetLocal n v -> do
        S_Seq (SetLocal n v) <$> k s ()

      Eff.GetByte a -> k s (GetByte a)
      Eff.SetByte a b -> S_Seq (SetByte a b) <$> k s ()

      -- TODO: explore maintaining temporary-stack at compile-time
      Eff.PushStack v -> do
        S_Seq (PushStack v) <$> k s ()

      Eff.PopStack -> do
        u <- GenUnique
        let name :: Identifier Value = Identifier u
        let v :: Expression Value = Variable name
        S_Seq (PopStack name) <$> k s v

      Eff.Random range -> do
        undefined range

      Eff.Quit -> do
        undefined

      Eff.If pred -> do
        case pred of
          Const pred -> k s pred
          _ -> do
            b1 <- k s True
            b2 <- k s False
            pure $ S_If pred b1 b2

      Eff.Foreach xs f -> do
        undefined xs f

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
      Eff.ShiftR x y -> prim2 x (Const y) Prim.ShiftR -- TODO: avoid special case
      Eff.Sub x y -> prim2 x y Prim.Sub
      Eff.TestBit x y -> prim2 x y Prim.TestBit

      Eff.LookupInStrings{} -> undefined
      Eff.StringBytes{} -> undefined
      Eff.Tokenize{} -> undefined

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
  = SetByte (Expression Addr) (Expression Byte)
  | GamePrint (Expression String)
  | TraceOperation (Expression Addr) Operation
  | TraceRoutineCall (Expression Addr)
  | PushFrame
  | PushReturnAddress (Expression Addr) -- TODO: needs to be an expression?
  | PushStack (Expression Value)
  | PopStack (Identifier Value)
  | SetLocal (Expression Byte) (Expression Value)
  deriving Show

data Expression a where
  Const :: a -> Expression a
  NumActuals :: Expression Byte
  CallResult :: Expression Value
  Variable :: Identifier a -> Expression a
  Unary :: Show x => Prim.P1 x r -> Expression x -> Expression r
  Binary :: (Show x, Show y) => Prim.P2 x y r -> Expression x -> Expression y -> Expression r
  GetByte :: Expression Addr -> Expression Byte
  List :: Show x => [Expression x] -> Expression [x]

instance Show a => Show (Expression a) where
  show = \case
    Const a -> show a
    NumActuals -> "num_actuals"
    CallResult -> "call_result"
    Variable v -> show v
    Unary p1 x -> show p1 ++ "(" ++ show x ++ ")"
    Binary p2 x y -> show p2 ++ "(" ++ show x ++ "," ++ show y ++ ")"
    GetByte a -> "M[" ++ show a ++ "]"
    List xs -> "List(" ++ intercalate "," (map show xs) ++ ")"

data Identifier a where
  Identifier :: Int -> Identifier a

instance Show (Identifier a) where
  show (Identifier i) = "v_" ++ show i
