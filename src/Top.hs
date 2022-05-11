
module Top (main)  where

import Dis (disassemble)
import Story (Story,loadStory)
import System.Environment (getArgs)
import Walk (traceExecution,dumpObjects)

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
  [] -> Trace
  args -> error (show ("parse",args))

data Config = Dis | Objects | Trace

run :: Story -> Config -> IO ()
run story = \case
  Dis -> disassemble story
  Objects -> dumpObjects story
  Trace -> traceExecution story
