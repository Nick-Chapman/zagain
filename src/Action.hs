
-- | The (inter)action of z-machine execution with input/output.
module Action (Conf(..),Action(..),StatusLine(..)) where

import Numbers (Addr,Style)
import Operation (Operation)

data Conf = Conf
  { debug :: Bool
  , seeTrace :: Bool
  , niz :: Bool
  , mojo :: Bool
  , frotz :: Bool
  , showInput :: Bool
  , bufferOutput :: Bool
  , wrapSpec :: Maybe Int
  }

data StatusLine = StatusLine { left :: String, right :: String }

data Action
  = TraceInstruction String Int Addr Operation Action -- TODO: kill state-string
  | Tab Action
  | UnTab Action
  | TraceRoutineCall Addr Action
  | Output String Action
  | Debug String Action
  | Input (Maybe StatusLine) Int (String -> Action)
  | Stop Int
  | TextStyle (Style,Bool) Action
