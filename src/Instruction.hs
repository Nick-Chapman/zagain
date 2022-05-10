
module Instruction
  ( Instruction(..)
  , Func(..)
  , Args(..)
  , Arg(..)
  , Target(..)
  , Label(..)
  , Boolean(..)
  , Dest(..)
  , pretty
  , RoutineHeader(..)
  ) where

import Data.List (intercalate)
import Numbers (Byte,Addr,Value)
import Prelude hiding (Word)
import Text.Printf (printf)

data Instruction -- TODO: check naming matches spec
  = Bad String

  -- This instructions are not yet decoded
  -- | Aread Arg Arg Target
  -- | CallN Func Args
  -- | Get_next_prop Arg Arg Target
  -- | Input_Stream Arg
  -- | Load Arg Target
  -- | Mod_ Arg Arg Target
  -- | Or_ Arg Arg Target
  -- | Output_Stream Arg (Maybe Arg)
  -- | Quit
  -- | Remove_obj Arg
  -- | Restart
  -- | Restore_lab Label
  -- | Save_lab Label
  -- | Show_status
  -- | Verify Label
  | Add Arg Arg Target
  | And_ Arg Arg Target
  | Call Func Args Target
  | Clear_attr Arg Arg
  | Dec Arg
  | Dec_check Arg Arg Label
  | Div Arg Arg Target
  | Get_child Arg Target Label
  | Get_parent Arg Target
  | Get_prop Arg Arg Target
  | Get_prop_addr Arg Arg Target
  | Get_prop_len Arg Target
  | Get_sibling Arg Target Label
  | Inc Arg
  | Inc_check Arg Arg Label
  | Insert_obj Arg Arg
  | Je Args Label
  | Jg Arg Arg Label
  | Jin Arg Arg Label
  | Jl Arg Arg Label
  | Jump Addr
  | Jz Arg Label
  | Load_byte Arg Arg Target
  | Load_word Arg Arg Target
  | Mul Arg Arg Target
  | New_line
  | Print String
  | Print_addr Arg
  | Print_char Arg
  | Print_num Arg
  | Print_obj Arg
  | Print_paddr Arg
  | Print_ret String
  | Pull Arg
  | Push Arg
  | Put_prop Arg Arg Arg
  | Random Arg Target
  | Ret_popped
  | Return Arg
  | Rfalse
  | Rtrue
  | Set_attr Arg Arg
  | Sread Arg Arg
  | Store Arg Arg
  | Storeb Arg Arg Arg
  | Storew Arg Arg Arg
  | Sub Arg Arg Target
  | Test Arg Arg Label
  | Test_attr Arg Arg Label

  deriving Show

newtype Args = Args [Arg] -- TODO: deprecate (changes regression)

instance Show Args where
  show (Args xs) = printf "(%s)" (intercalate " " (map (printf "(%s)" . show) xs))

data Func = Floc Addr | Fvar Target
  deriving Show

data Arg = Con Value | Var Target
  deriving Show

data Target = Sp | Local Byte | Global Byte
  deriving Show

data Label = Branch Boolean Dest -- TODO: prefer Sense to Boolean
  deriving Show

data Dest = Dfalse | Dtrue | Dloc Addr
  deriving Show

data Boolean = T | F -- TODO: deprecate (changes regression)

instance Show Boolean where show = \case T -> "true"; F -> "false"

pretty :: Instruction -> String
pretty i = bracket i (show i)

bracket :: Instruction -> String -> String
bracket i = if needBracket i then printf "(%s)" else id
  where
    needBracket = \case -- TODO: deprecate (changes regression)
      New_line -> False
      Ret_popped -> False
      Rfalse -> False
      Rtrue -> False
      _ -> True

data RoutineHeader = RoutineHeader [Value]

instance Show RoutineHeader where
  show (RoutineHeader xs) =
    printf "((var_initializations (%s)))" (intercalate " " (map show xs))

