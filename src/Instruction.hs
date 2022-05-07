
module Instruction
  ( Instruction(..)
  , Func(..)
  , Args(..)
  , Arg(..)
  , Variable(..)
  , Label(..)
  , Boolean(..)
  , Dest(..)
  , pretty
  , RoutineHeader(..)
  ) where

import Addr (Addr)
import Data.List (intercalate)
import Data.Word (Word8)
import Text.Printf (printf)

type Byte = Word8

data Instruction -- TODO: check naming matches spec
  = Bad String

  -- This instructions are not yet decoded
  -- | Aread Arg Arg Variable
  -- | CallN Func Args
  -- | Get_next_prop Arg Arg Variable
  -- | Input_Stream Arg
  -- | Load Arg Variable
  -- | Mod_ Arg Arg Variable
  -- | Or_ Arg Arg Variable
  -- | Output_Stream Arg (Maybe Arg)
  -- | Quit
  -- | Remove_obj Arg
  -- | Restart
  -- | Restore_lab Label
  -- | Save_lab Label
  -- | Show_status
  -- | Verify Label
  | Add Arg Arg Variable
  | And_ Arg Arg Variable
  | Call Func Args Variable
  | Clear_attr Arg Arg
  | Dec Arg
  | Dec_check Arg Arg Label
  | Div Arg Arg Variable
  | Get_child Arg Variable Label
  | Get_parent Arg Variable
  | Get_prop Arg Arg Variable
  | Get_prop_addr Arg Arg Variable
  | Get_prop_len Arg Variable
  | Get_sibling Arg Variable Label
  | Inc Arg
  | Inc_check Arg Arg Label
  | Insert_obj Arg Arg
  | Je Args Label
  | Jg Arg Arg Label
  | Jin Arg Arg Label
  | Jl Arg Arg Label
  | Jump Addr
  | Jz Arg Label
  | Load_byte Arg Arg Variable
  | Load_word Arg Arg Variable
  | Mul Arg Arg Variable
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
  | Random Arg Variable
  | Ret_popped
  | Return Arg
  | Rfalse
  | Rtrue
  | Set_attr Arg Arg
  | Sread Arg Arg
  | Store Arg Arg
  | Storeb Arg Arg Arg
  | Storew Arg Arg Arg
  | Sub Arg Arg Variable
  | Test Arg Arg Label
  | Test_attr Arg Arg Label

  deriving Show

newtype Args = Args [Arg] -- TODO: deprecate (changes regression)

instance Show Args where
  show (Args xs) = printf "(%s)" (intercalate " " (map (printf "(%s)" . show) xs))

data Func = Floc Addr | Fvar Variable
  deriving Show

data Arg = Con Int | Var Variable
  deriving Show

data Variable = Sp | Local Byte | Global Byte
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

data RoutineHeader = RoutineHeader [Int]

instance Show RoutineHeader where
  show (RoutineHeader xs) =
    printf "((var_initializations (%s)))" (intercalate " " (map show xs))

