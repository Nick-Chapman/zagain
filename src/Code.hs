
module Code
  ( Code(..), CompiledRoutine(..), Chunk(..)
  , Prog(..), Atom(..), Binding(..), Label(..), Loc(..)
  , Expression(..), Identifier(..)
  , dumpCode
  )
where

import Data.List (intercalate)
import Numbers (Addr,Byte,Value)
import Operation (Operation)
import Text.Printf (printf)
import qualified Eff (StatusInfo(..))
import qualified Primitive as Prim

--[code]--------------------------------------------------------------

dumpCode :: Code -> IO ()
dumpCode Code{routines} = do
  printf "Code for %d routines\n" (length routines)
  sequence_
    [ do
        printf "--------------------------------------------------\n"
        mapM_ print chunks
    | CompiledRoutine{chunks} <- routines
    ]

data Code = Code
  { routines :: [CompiledRoutine]
  }

data CompiledRoutine = CompiledRoutine
  { chunks :: [Chunk]
  }

data Chunk = Chunk { label :: Loc Addr, body :: Prog }

instance Show Chunk where
  show Chunk{ label, body } =
    intercalate "\n" ((show label ++ ":") : pretty 2 body)

data Loc a = LocRoutine a | LocOp a | LocReturn a
  deriving (Eq,Ord,Show)

seeLoc :: Show a => Loc a -> String
seeLoc = \case
  LocOp a -> show a
  LocReturn a -> "(return) " ++ show a
  LocRoutine a -> "(routine) " ++ show a

newtype Label = Label Int
  deriving Show

data Prog where
  Null :: Prog
  Quit :: Prog
  Error :: String -> Prog
  Labelled :: Label -> Prog -> Prog
  Goto :: Label -> Prog
  JumpIndirect :: Loc (Expression Addr) -> Prog
  Jump :: Loc Addr -> Prog
  Seq :: Atom ->  Prog -> Prog
  FullSeq :: Prog -> Prog -> Prog
  If :: Expression Bool -> Prog -> Prog -> Prog
  deriving Show

pretty :: Int -> Prog -> [String]
pretty i = \case
  Null -> []
  Quit -> [tab i "Quit"]
  Error msg -> [tab i ("Error: " ++ msg)]
  Labelled label prog -> concat
    [ [tab i (show label ++ ": {") ]
    , pretty (i+2) prog
    , [tab i "}"]
    ]
  Goto label ->
    [tab i ("Goto: " ++ show label)]
  JumpIndirect a ->
    [tab i ("JumpIndirect: " ++ seeLoc a)]
  Jump a ->
    [tab i ("Jump: " ++ seeLoc a)]
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
    , pretty (i+2) s2
    , [tab i "}"]
    ]

tab :: Int -> String -> String
tab n s = take n (repeat ' ') ++ s

data Atom
  = SetByte (Expression Addr) (Expression Byte)
  | Note String
  | TraceOperation (Expression Addr) Operation
  | GamePrint (Expression String)
  | MakeRoutineFrame Int
  | PushFrame
  | PopFrame
  | PushReturnAddress (Expression Addr)
  | PopReturnAddress (Identifier Addr)
  | PushStack (Expression Value)
  | PopStack (Identifier Value)
  | SetLocal (Expression Byte) (Expression Value)
  | ReadInputFromUser StatusInfo (Identifier String)
  | StringBytes (Expression String) (Identifier [Expression Byte])
  | Tokenize (Expression String) TokenizeIdents
  | LetRandom (Identifier Value) (Expression Value)
  | Let Binding
  | Assign Binding
  | SetNumberActuals (Expression Byte)
  | SetResult (Expression Value)
  deriving Show

data Binding where
  Binding :: Show x => Identifier x -> Expression x -> Binding

instance Show Binding where
  show = \case
    Binding x e -> show x ++ " = " ++ show e

type StatusInfo = Maybe (Eff.StatusInfo (Expression String) (Expression Value))

type TokenizeIdents =
  ( Identifier Byte
  , Identifier [Expression Byte]
  , Identifier [Expression String]
  )

data Expression a where
  Join :: Expression (Expression a) -> Expression a
  Const :: a -> Expression a
  NumActuals :: Expression Byte
  CallResult :: Expression Value
  Variable :: Identifier a -> Expression a
  Unary :: Show x => Prim.P1 x r -> Expression x -> Expression r
  Binary :: (Show x, Show y) => Prim.P2 x y r -> Expression x -> Expression y -> Expression r
  GetByteE :: Expression Addr -> Expression Byte
  GetLocalE :: Expression Byte -> Expression Value
  GetTextE :: Expression Addr -> Expression String
  LookupInDictE :: Expression String -> Expression Addr
  Ite :: Expression Bool -> Expression a -> Expression a -> Expression a

instance Show a => Show (Expression a) where
  show = \case
    Join e -> show e
    Const a -> show a
    NumActuals -> "num_actuals"
    CallResult -> "call_result"
    Variable v -> show v
    Unary p1 x -> show p1 ++ "(" ++ show x ++ ")"
    Binary p2 x y -> show p2 ++ "(" ++ show x ++ "," ++ show y ++ ")"
    GetByteE a -> "M[" ++ show a ++ "]"
    GetLocalE n -> "GetLocal(" ++ show n ++ ")"
    GetTextE a -> "GetText(" ++ show a ++ ")"
    LookupInDictE x -> "LookupInDict(" ++ show x ++ ")"
    Ite i t e -> "Ite(" ++ show (i,t,e) ++ ")"

data Identifier a where
  Identifier :: String -> Int -> Identifier a

instance Show (Identifier a) where
  show (Identifier tag i) = tag ++ show i
