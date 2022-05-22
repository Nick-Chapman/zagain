
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

data Operation
  = BadOperation String

  | Add Arg Arg Target
  | And Arg Arg Target
  | Call Func [Arg] Target
  | Clear_attr Arg Arg
  | Dec Arg
  | Dec_chk Arg Arg Label
  | Div Arg Arg Target
  | Get_child Arg Target Label
  | Get_next_prop Arg Arg Target
  | Get_parent Arg Target
  | Get_prop Arg Arg Target
  | Get_prop_addr Arg Arg Target
  | Get_prop_len Arg Target
  | Get_sibling Arg Target Label
  | Inc Arg
  | Inc_chk Arg Arg Label
  | Input_stream Arg
  | Insert_obj Arg Arg
  | Je [Arg] Label
  | Jg Arg Arg Label
  | Jin Arg Arg Label
  | Jl Arg Arg Label
  | Jump Addr
  | Jz Arg Label
  | Load Arg Target
  | Loadb Arg Arg Target
  | Loadw Arg Arg Target
  | Mod Arg Arg Target
  | Mul Arg Arg Target
  | New_line
  | Nop
  | Or Arg Arg Target
  | Output_stream Arg
  | Pop
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
  | Restart
  | Restore Label
  | Ret_popped
  | Ret Arg
  | Rfalse
  | Rtrue
  | Save Label
  | Set_attr Arg Arg
  | Set_window Arg
  | Show_status
  | Split_window Arg
  | Sread Arg Arg
  | Store Arg Arg
  | Storeb Arg Arg Arg
  | Storew Arg Arg Arg
  | Sub Arg Arg Target
  | Test Arg Arg Label
  | Test_attr Arg Arg Label
  | Verify Label

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
