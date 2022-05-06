
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
  ) where

import Addr (Addr)
import Data.List (intercalate)
import Data.Word (Word8)
import Text.Printf (printf)

type Byte = Word8

data Instruction
  = Call Func Args Variable
  | Storew Arg Arg Arg
  | Put_prop Arg Arg Arg
  | Add Arg Arg Variable
  | Store Arg Arg
  | Test_attr Arg Arg Label
  | New_line
  | Insert_obj Arg Arg
  | Jump Addr
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
