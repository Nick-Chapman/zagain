
-- | The (inter)action of z-machine execution with input/output.
module Action (Conf(..),Action(..)) where

import Numbers (Addr)
import Operation (Operation)

data Conf = Conf
  { debug :: Bool
  , seeTrace :: Bool
  , mojo :: Bool
  , bufferOutput :: Bool
  , wrap :: Maybe Int
  }

data Action
  = TraceInstruction String Int Addr Operation Action
  | Output String Action
  | Debug String Action
  | Input (String,String) Int (String -> Action)
  | Stop Int
