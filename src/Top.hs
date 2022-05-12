
module Top (main)  where

import Dis (disassemble)
import Interaction (Inter,runInter)
import Story (Story,loadStory)
import System.Environment (getArgs)
import Walk (State,initState,runEff)
import qualified Evaluation (theEffect)
import qualified Interaction (Conf(..))
import qualified Objects (dump)

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


traceExecution :: Interaction.Conf -> Story -> [String] -> IO ()
traceExecution conf story inputs = do
  let maxSteps = 395
  let e = Evaluation.theEffect
  let s :: State = initState story
  let i :: Inter = runEff maxSteps s e
  runInter conf inputs i

dumpObjects :: Story -> IO ()
dumpObjects story = do
  let maxSteps = 1000
  let e = Objects.dump
  let s :: State = initState story
  let i :: Inter = runEff maxSteps s e
  let conf = Interaction.Conf { debug = True, seeStats = False }
  runInter conf [] i
