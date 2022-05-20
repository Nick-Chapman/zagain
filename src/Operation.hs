
-- | Decoded z-machine operations.
module Operation
  ( Operation(..)
  , Func(..)
  , Arg(..)
  , Target(..)
  , Label(..)
  , Sense(..)
  , Dest(..)
  , RoutineHeader(..)
  ) where

import Numbers (Byte,Addr,Value)

data Operation -- TODO: check naming matches spec
  = BadOperation String

  | Add Arg Arg Target
  | And_ Arg Arg Target
  | Call Func [Arg] Target
  | Clear_attr Arg Arg
  | Dec Arg
  | Dec_check Arg Arg Label
  | Div Arg Arg Target
  | Get_child Arg Target Label
  | Get_next_prop Arg Arg Target
  | Get_parent Arg Target
  | Get_prop Arg Arg Target
  | Get_prop_addr Arg Arg Target
  | Get_prop_len Arg Target
  | Get_sibling Arg Target Label
  | Inc Arg
  | Inc_check Arg Arg Label
  | Insert_obj Arg Arg
  | Je [Arg] Label
  | Jg Arg Arg Label
  | Jin Arg Arg Label
  | Jl Arg Arg Label
  | Jump Addr
  | Jz Arg Label
  | Load Arg Target
  | Load_byte Arg Arg Target
  | Load_word Arg Arg Target
  | Mod Arg Arg Target
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
  | Quit
  | Random Arg Target
  | Remove_obj Arg
  | Restore_lab Label
  | Ret_popped
  | Return Arg
  | Rfalse
  | Rtrue
  | Save_lab Label
  | Set_attr Arg Arg
  | Sread Arg Arg
  | Store Arg Arg
  | Storeb Arg Arg Arg
  | Storew Arg Arg Arg
  | Sub Arg Arg Target
  | Test Arg Arg Label
  | Test_attr Arg Arg Label

  -- This instructions are not yet decoded
  -- | Aread Arg Arg Target
  -- | CallN Func Args
  -- | Input_Stream Arg
  -- | Or_ Arg Arg Target
  -- | Output_Stream Arg (Maybe Arg)
  -- | Restart
  -- | Verify Label
  -- | Show_status
  -- | Restart

  deriving Show

data Func = Floc Addr | Fvar Target | BadFunc
  deriving Show

data Arg = Con Value | Var Target
  deriving Show

data Target = Sp | Local Byte | Global Byte
  deriving Show

data Label = Branch Sense Dest
  deriving Show

data Dest = Dfalse | Dtrue | Dloc Addr
  deriving Show

data Sense = T | F
  deriving Show

data RoutineHeader = BadRoutineHeader | RoutineHeader [Value]
  deriving Show
