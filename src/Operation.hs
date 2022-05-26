
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
  , opLocals
  , opTargets
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

----------------------------------------------------------------------

opLocals :: Operation -> [Byte]
opLocals op = [ b | t <- opTargets op, Local b <- [t] ]

opTargets :: Operation -> [Target]
opTargets op =
  case opTargetOpt op of
    Just t -> t : [ t | arg <- opArgs op, Var t <- [arg] ]
    Nothing -> [ t | arg <- opArgs op, Var t <- [arg] ]

opArgs :: Operation -> [Arg]
opArgs = \case
  BadOperation _mes -> do []
  Add arg1 arg2 _target -> do [arg1,arg2]
  And arg1 arg2 _target -> do [arg1,arg2]
  Call _func args _target -> do args
  Clear_attr arg1 arg2 -> do [arg1,arg2]
  Dec arg -> do [arg]
  Dec_chk arg1 arg2 _label -> do [arg1,arg2]
  Div arg1 arg2 _target -> do [arg1,arg2]
  Get_child arg _target _label -> do [arg]
  Get_next_prop arg1 arg2 _target -> do [arg1,arg2]
  Get_parent arg _target -> do [arg]
  Get_prop arg1 arg2 _target -> do [arg1,arg2]
  Get_prop_addr arg1 arg2 _target -> do [arg1,arg2]
  Get_prop_len arg _target -> do [arg]
  Get_sibling arg _target _label -> do [arg]
  Inc arg -> do [arg]
  Inc_chk arg1 arg2 _label -> do [arg1,arg2]
  Input_stream arg -> do [arg]
  Insert_obj arg1 arg2 -> do [arg1,arg2]
  Je args _label -> do args
  Jg arg1 arg2 _label -> do [arg1,arg2]
  Jin arg1 arg2 _label -> do [arg1,arg2]
  Jl arg1 arg2 _label -> do [arg1,arg2]
  Jump _addr -> do []
  Jz arg _label -> do [arg]
  Load arg _target -> do [arg]
  Loadb arg1 arg2 _target -> do [arg1,arg2]
  Loadw arg1 arg2 _target -> do [arg1,arg2]
  Mod arg1 arg2 _target -> do [arg1,arg2]
  Mul arg1 arg2 _target -> do [arg1,arg2]
  New_line -> do []
  Nop -> do []
  Or arg1 arg2 _target -> do [arg1,arg2]
  Output_stream arg -> do [arg]
  Pop -> do []
  Print _string -> do []
  Print_addr arg -> do [arg]
  Print_char arg -> do [arg]
  Print_num arg -> do [arg]
  Print_obj arg -> do [arg]
  Print_paddr arg -> do [arg]
  Print_ret _string -> do []
  Pull arg -> do [arg]
  Push arg -> do [arg]
  Put_prop arg1 arg2 arg3 -> do [arg1,arg2,arg3]
  Quit -> do []
  Random arg _target -> do [arg]
  Remove_obj arg -> do [arg]
  Restart -> do []
  Restore _label -> do []
  Ret arg -> do [arg]
  Ret_popped -> do []
  Rfalse -> do []
  Rtrue -> do []
  Save _label -> do []
  Set_attr arg1 arg2 -> do [arg1,arg2]
  Set_window arg -> do [arg]
  Show_status-> do []
  Split_window arg -> do [arg]
  Sread arg1 arg2 -> do [arg1,arg2]
  Store arg1 arg2 -> do [arg1,arg2]
  Storeb arg1 arg2 arg3 -> do [arg1,arg2,arg3]
  Storew arg1 arg2 arg3 -> do [arg1,arg2,arg3]
  Sub arg1 arg2 _target -> do [arg1,arg2]
  Test arg1 arg2 _label -> do [arg1,arg2]
  Test_attr arg1 arg2 _label -> do [arg1,arg2]
  Verify _label -> do []


opTargetOpt :: Operation -> Maybe Target
opTargetOpt = \case
  BadOperation _mes -> do Nothing
  Add _arg1 _arg2 target -> do Just target
  And _arg1 _arg2 target -> do Just target
  Call _func _args target -> do Just target
  Clear_attr _arg1 _arg2 -> do Nothing
  Dec _arg -> do Nothing
  Dec_chk _arg1 _arg2 _label -> do Nothing
  Div _arg1 _arg2 target -> do Just target
  Get_child _arg target _label -> do Just target
  Get_next_prop _arg1 _arg2 target -> do Just target
  Get_parent _arg target -> do Just target
  Get_prop _arg1 _arg2 target -> do Just target
  Get_prop_addr _arg1 _arg2 target -> do Just target
  Get_prop_len _arg target -> do Just target
  Get_sibling _arg target _label -> do Just target
  Inc _arg -> do Nothing
  Inc_chk _arg1 _arg2 _label -> do Nothing
  Input_stream _arg -> do Nothing
  Insert_obj _arg1 _arg2 -> do Nothing
  Je _args _label -> do Nothing
  Jg _arg1 _arg2 _label -> do Nothing
  Jin _arg1 _arg2 _label -> do Nothing
  Jl _arg1 _arg2 _label -> do Nothing
  Jump _addr -> do Nothing
  Jz _arg _label -> do Nothing
  Load _arg target -> do Just target
  Loadb _arg1 _arg2 target -> do Just target
  Loadw _arg1 _arg2 target -> do Just target
  Mod _arg1 _arg2 target -> do Just target
  Mul _arg1 _arg2 target -> do Just target
  New_line -> do Nothing
  Nop -> do Nothing
  Or _arg1 _arg2 target -> do Just target
  Output_stream _arg -> do Nothing
  Pop -> do Nothing
  Print _string -> do Nothing
  Print_addr _arg -> do Nothing
  Print_char _arg -> do Nothing
  Print_num _arg -> do Nothing
  Print_obj _arg -> do Nothing
  Print_paddr _arg -> do Nothing
  Print_ret _string -> do Nothing
  Pull _arg -> do Nothing
  Push _arg -> do Nothing
  Put_prop _arg1 _arg2 _arg3 -> do Nothing
  Quit -> do Nothing
  Random _arg target -> do Just target
  Remove_obj _arg -> do Nothing
  Restart -> do Nothing
  Restore _label -> do Nothing
  Ret _arg -> do Nothing
  Ret_popped -> do Nothing
  Rfalse -> do Nothing
  Rtrue -> do Nothing
  Save _label -> do Nothing
  Set_attr _arg1 _arg2 -> do Nothing
  Set_window _arg -> do Nothing
  Show_status-> do Nothing
  Split_window _arg -> do Nothing
  Sread _arg1 _arg2 -> do Nothing
  Store _arg1 _arg2 -> do Nothing
  Storeb _arg1 _arg2 _arg3 -> do Nothing
  Storew _arg1 _arg2 _arg3 -> do Nothing
  Sub _arg1 _arg2 target -> do Just target
  Test _arg1 _arg2 _label -> do Nothing
  Test_attr _arg1 _arg2 _label -> do Nothing
  Verify _label -> do Nothing

