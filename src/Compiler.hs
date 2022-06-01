
module Compiler (compileEffect,runCode) where

import Action (Action)
import Eff (Eff(..),Phase(..))
import Story (Story)

data Compile

instance Phase Compile where
  type Addr Compile = ()
  type Byte Compile = ()
  type Pred Compile = ()
  type Text Compile = ()
  type Value Compile = ()
  type Vector Compile a = [a]

compileEffect :: Story -> Eff Compile () -> IO Code
compileEffect = undefined

data Code

runCode :: Word -> Code -> Action
runCode = undefined
