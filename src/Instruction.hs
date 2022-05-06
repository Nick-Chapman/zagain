
module Instruction
  ( Instruction(..)
  , Func(..)
  , Arg(..)
  , Variable(..)
  , Label(..)
  , Dest(..)
  ) where

import Data.Word (Word8)

type Byte = Word8
type Addr = Int

data Instruction
  = Call Func [Arg] Variable
  | StoreW Arg Arg Arg
  | PutProp Arg Arg Arg
  | Add Arg Arg Variable
  | Store Arg Arg
  | TestAttr Arg Arg Label
  | Newline
  | InsertObj Arg Arg
  | Jump Addr
  deriving Show

data Func = FuncA Addr | FuncV Variable
  deriving Show

data Arg = Con Int | Var Variable
  deriving Show

data Variable = Sp | Local Byte | Global Byte
  deriving Show

data Label = Branch Bool Dest
  deriving Show

data Dest = Dfalse | Dtrue | Dloc Addr
  deriving Show
