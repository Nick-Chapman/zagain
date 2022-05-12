
module Top (main)  where

import Dis (disassemble)
import Story (Story,loadStory)
import System.Environment (getArgs)
import Walk (traceExecution,dumpObjects)
import qualified Interaction (Conf(..))

main :: IO ()
main = do
  args <- getArgs
  let config = parseCommandLine args
  story <- loadStory "story/zork1.88-840726.z3"
  run story config

parseCommandLine :: [String] -> Config
parseCommandLine = \case
  ["dis"] -> Dis
  ["objects"] -> Objects
  ["trace"] -> Trace
  [] -> Dev
  args -> error (show ("parse",args))

data Config = Dis | Objects | Trace | Dev

run :: Story -> Config -> IO ()
run story = \case
  Dis ->
    disassemble story
  Objects ->
    dumpObjects story
  Trace -> do
    let conf = Interaction.Conf { debug = True, seeStats = False }
    traceExecution conf story []
  Dev -> do
    let conf = Interaction.Conf { debug = True, seeStats = True }
    traceExecution conf story ["open mailbox"]
