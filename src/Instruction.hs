
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

data Instruction
  = Bad String

  | Add Arg Arg Variable
  | Call Func Args Variable
  | Insert_obj Arg Arg
  | Je Args Label
  | Jump Addr
  | New_line
  | Print String
  | Print_ret String
  | Put_prop Arg Arg Arg
  | Store Arg Arg
  | Storew Arg Arg Arg
  | Test_attr Arg Arg Label

  | And_ Arg Arg Variable
  | Aread Arg Arg Variable
  | CallN Func Args
  | Clear_attr Arg Arg
  | Dec Arg
  | Dec_check Arg Arg Label
  | Div Arg Arg Variable
  | Get_child Arg Variable Label
  | Get_next_prop Arg Arg Variable
  | Get_parent Arg Variable
  | Get_prop Arg Arg Variable
  | Get_prop_addr Arg Arg Variable
  | Get_prop_len Arg Variable
  | Get_sibling Arg Variable Label
  | Inc Arg
  | Inc_check Arg Arg Label
  | Input_Stream Arg
  | Jg Arg Arg Label
  | Jin Arg Arg Label
  | Jl Arg Arg Label
  | Jz Arg Label
  | Load Arg Variable
  | Load_byte Arg Arg Variable
  | Load_word Arg Arg Variable
  | Mod_ Arg Arg Variable
  | Mul Arg Arg Variable
  | Or_ Arg Arg Variable
  | Output_Stream Arg (Maybe Arg)
  | Print_addr Arg
  | Print_char Arg
  | Print_num Arg
  | Print_obj Arg
  | Print_paddr Arg
  | Push Arg
  | Quit
  | Random Arg Variable
  | Remove_obj Arg
  | Restart
  | Restore_lab Label
  | Ret_popped
  | Return Arg
  | Rfalse
  | Rtrue
  | Save_lab Label
  | Set_attr Arg Arg
  | Show_status
  | Sread Arg Arg
  | Storeb Arg Arg Arg
  | Sub Arg Arg Variable
  | Test Arg Arg Label
  | Verify Label

  deriving Show


newtype Args = Args [Arg]

instance Show Args where
  show (Args xs) = printf "(%s)" (intercalate " " (map (printf "(%s)" . show) xs))

data Func = Floc Addr | FuncV Variable
  deriving Show

data Arg = Con Int | Var Variable
  deriving Show

data Variable = Sp | Local Byte | Global Byte
  deriving Show

data Label = Branch Boolean Dest
  deriving Show

data Dest = Dfalse | Dtrue | Dloc Addr
  deriving Show

data Boolean = T | F

instance Show Boolean where show = \case T -> "true"; F -> "false"

pretty :: Instruction -> String
pretty i = bracket i (show i)

bracket :: Instruction -> String -> String
bracket i = if needBracket i then printf "(%s)" else id
  where
    needBracket = \case
      New_line -> False
      _ -> True

data RoutineHeader = RoutineHeader [Int]

instance Show RoutineHeader where
  show (RoutineHeader xs) =
    printf "((var_initializations (%s)))" (intercalate " " (map show xs))

