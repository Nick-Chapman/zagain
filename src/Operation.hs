
-- | Decoded z-machine operations.
module Operation
  ( Operation(..)
  , Func(..)
  , Args(..)
  , Arg(..)
  , Target(..)
  , Label(..)
  , Sense(..)
  , Dest(..)
  , RoutineHeader(..)
  , opLocals
  , opTargets
  , opLabels
  , showOp
  ) where

import Data.List (intercalate)
import Numbers (Byte,Addr,Value)

niz :: Bool
niz = False

data Operation
  = BadOperation String

  | Add Arg Arg Target
  | And Arg Arg Target -- And_ in niz
  | Aread Arg Arg Target
  | Buffer_mode Arg
  | Call Func Args Target
  | CallN Func Args
  | Check_arg_count Arg Label
  | Clear_attr Arg Arg
  | Dec Arg
  | Dec_chk Arg Arg Label -- Dec_check in niz
  | Div Arg Arg Target
  | Erase_window Arg
  | Get_child Arg Target Label
  | Get_next_prop Arg Arg Target
  | Get_parent Arg Target
  | Get_prop Arg Arg Target
  | Get_prop_addr Arg Arg Target
  | Get_prop_len Arg Target
  | Get_sibling Arg Target Label
  | Inc Arg
  | Inc_chk Arg Arg Label -- Inc_check in niz
  | Input_stream Arg
  | Insert_obj Arg Arg
  | Je Args Label
  | Jg Arg Arg Label
  | Jin Arg Arg Label
  | Jl Arg Arg Label
  | Jump Addr
  | Jz Arg Label
  | Load Arg Target
  | Loadb Arg Arg Target -- Load_byte in niz
  | Loadw Arg Arg Target -- Load_word in niz
  | Mod Arg Arg Target
  | Mul Arg Arg Target
  | New_line
  | Nop
  | Not Arg Target
  | Or Arg Arg Target
  | Output_stream1 Arg
  | Output_stream2 Arg Arg
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
  | Read_char Target
  | Remove_obj Arg
  | Restart
  | Restore Label
  | Ret Arg -- Return in niz
  | Ret_popped
  | Rfalse
  | Rtrue
  | Save Label
--  | Save_target Target -- TODO: Z4
  | Save_undo Target
  | Scan_table Arg Arg Arg Target Label
  | Set_attr Arg Arg
  | Set_colour Arg Arg
  | Set_cursor Arg Arg
  | Set_text_style Arg
  | Set_window Arg
  | Show_status
  | Sound_effect1 Arg
  | Sound_effect Arg Arg Arg
  | Split_window Arg
  | Sread Arg Arg
  | Store Arg Arg
  | Storeb Arg Arg Arg
  | Storew Arg Arg Arg
  | Sub Arg Arg Target
  | Test Arg Arg Label
  | Test_attr Arg Arg Label
  | Tokenize Arg Arg
  | Verify Label

  deriving Show

data Func = Floc Addr | Fvar Target | BadFunc
  deriving Show

newtype Args = Args [Arg]

instance Show Args where
  show (Args xs) = if niz then showArgsForNiz xs else show xs
    where
      showArgsForNiz :: [Arg] -> String
      showArgsForNiz xs =
        "(" ++ intercalate " " (map seeArg xs) ++ ")"
        where seeArg x = "(" ++ show x ++ ")"

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
--instance Show Sense where show = \case T -> "true"; F -> "false"

data RoutineHeader = BadRoutineHeader | RoutineHeader [Value]
  deriving Show


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
  Aread arg1 arg2 _target -> do [arg1,arg2]
  Buffer_mode arg -> do [arg]
  Call _func (Args args) _target -> do args
  CallN _func (Args args) -> do args
  Check_arg_count arg _label -> do [arg]
  Clear_attr arg1 arg2 -> do [arg1,arg2]
  Dec arg -> do [arg]
  Dec_chk arg1 arg2 _label -> do [arg1,arg2]
  Div arg1 arg2 _target -> do [arg1,arg2]
  Erase_window arg -> do [arg]
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
  Je (Args args) _label -> do args
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
  Not arg _target -> do [arg]
  Or arg1 arg2 _target -> do [arg1,arg2]
  Output_stream1 arg -> do [arg]
  Output_stream2 arg1 arg2 -> do [arg1,arg2]
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
  Read_char _target -> do []
  Remove_obj arg -> do [arg]
  Restart -> do []
  Restore _label -> do []
  Ret arg -> do [arg]
  Ret_popped -> do []
  Rfalse -> do []
  Rtrue -> do []
  Save _label -> do []
  Save_undo _target -> do []
  Scan_table arg1 arg2 arg3 _target _label -> do [arg1,arg2,arg3]
  Set_attr arg1 arg2 -> do [arg1,arg2]
  Set_colour arg1 arg2 -> do [arg1,arg2]
  Set_cursor arg1 arg2 -> do [arg1,arg2]
  Set_text_style arg -> do [arg]
  Set_window arg -> do [arg]
  Show_status -> do []
  Sound_effect1 arg -> do [arg]
  Sound_effect arg1 arg2 arg3 -> do [arg1,arg2,arg3]
  Split_window arg -> do [arg]
  Sread arg1 arg2 -> do [arg1,arg2]
  Store arg1 arg2 -> do [arg1,arg2]
  Storeb arg1 arg2 arg3 -> do [arg1,arg2,arg3]
  Storew arg1 arg2 arg3 -> do [arg1,arg2,arg3]
  Sub arg1 arg2 _target -> do [arg1,arg2]
  Test arg1 arg2 _label -> do [arg1,arg2]
  Test_attr arg1 arg2 _label -> do [arg1,arg2]
  Tokenize arg1 arg2 -> do [arg1,arg2]
  Verify _label -> do []


opTargetOpt :: Operation -> Maybe Target
opTargetOpt = \case
  BadOperation _mes -> do Nothing
  Add _arg1 _arg2 target -> do Just target
  And _arg1 _arg2 target -> do Just target
  Aread _arg1 _arg2 target -> do Just target
  Buffer_mode _arg -> do Nothing
  Call _func _args target -> do Just target
  CallN _func _args -> do Nothing
  Check_arg_count _arg _label -> do Nothing
  Clear_attr _arg1 _arg2 -> do Nothing
  Dec _arg -> do Nothing
  Dec_chk _arg1 _arg2 _label -> do Nothing
  Div _arg1 _arg2 target -> do Just target
  Erase_window _arg -> do Nothing
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
  Not _arg target -> do Just target
  Or _arg1 _arg2 target -> do Just target
  Output_stream1 _arg1 -> do Nothing
  Output_stream2 _arg1 _arg2 -> do Nothing
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
  Read_char target -> do Just target
  Remove_obj _arg -> do Nothing
  Restart -> do Nothing
  Restore _label -> do Nothing
  Ret _arg -> do Nothing
  Ret_popped -> do Nothing
  Rfalse -> do Nothing
  Rtrue -> do Nothing
  Save _label -> do Nothing
  Save_undo target -> do Just target
  Scan_table _arg1 _arg2 _arg3 target _label -> do Just target
  Set_attr _arg1 _arg2 -> do Nothing
  Set_colour _arg1 _arg2 -> do Nothing
  Set_cursor _arg1 _arg2 -> do Nothing
  Set_text_style _arg -> do Nothing
  Set_window _arg -> do Nothing
  Show_status-> do Nothing
  Sound_effect1 _arg -> do Nothing
  Sound_effect _arg1 _arg2 _arg3 -> do Nothing
  Split_window _arg -> do Nothing
  Sread _arg1 _arg2 -> do Nothing
  Store _arg1 _arg2 -> do Nothing
  Storeb _arg1 _arg2 _arg3 -> do Nothing
  Storew _arg1 _arg2 _arg3 -> do Nothing
  Sub _arg1 _arg2 target -> do Just target
  Test _arg1 _arg2 _label -> do Nothing
  Test_attr _arg1 _arg2 _label -> do Nothing
  Tokenize _arg1 _arg2 -> do Nothing
  Verify _label -> do Nothing


opLabels :: Operation -> [Label]
opLabels = \case
  Check_arg_count _arg label -> do [label]
  Dec_chk _arg1 _arg2 label -> do [label]
  Get_child _arg _target label -> do [label]
  Get_sibling _arg _target label -> do [label]
  Inc_chk _arg1 _arg2 label -> do [label]
  Je _args label -> do [label]
  Jg _arg1 _arg2 label -> do [label]
  Jin _arg1 _arg2 label -> do [label]
  Jl _arg1 _arg2 label -> do [label]
  Jz _arg label -> do [label]
  Restore label -> do [label]
  Scan_table _arg1 _arg2 _arg3 _target label -> do [label]
  Save label -> do [label]
  Test _arg1 _arg2 label -> do [label]
  Test_attr _arg1 _arg2 label -> do [label]
  Verify label -> do [label]
  _ -> do []


showOp :: Operation -> String
showOp = if niz then showOpForNiz else show

showOpForNiz :: Operation -> String
showOpForNiz op = -- TODO: map string for ops which are named differently in niz
  brac (show op)
  where
    brac s = if needBracket then "(" ++ s ++ ")" else s

    needBracket = case op of
      Rtrue -> False
      Rfalse -> False
      New_line -> False
      Ret_popped -> False
      _ -> True
