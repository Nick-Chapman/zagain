
module Top (main)  where

import Dictionary (fetchDict)
import Dis (disassemble)
import Fetch (runFetch)
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
  ["dict"] -> Dictionary
  ["trace"] -> Trace
  ["dev"] -> Dev
  [] -> Walk
  args -> error (show ("parse",args))

data Config = Dis | Objects | Dictionary | Trace | Walk | Dev

run :: Story -> Config -> IO ()
run story = \case
  Dis ->
    disassemble story
  Objects ->
    dumpObjects story
  Dictionary -> do
    let (dict,_,_) = runFetch 0 story fetchDict
    print dict
  Trace -> do -- for regression
    let conf = Interaction.Conf { debug = False, seeTrace = True, seeStats = False }
    traceExecution conf story []
  Walk -> do -- hide instruction trace
    let conf = Interaction.Conf { debug = True, seeTrace = False, seeStats = False }
    traceExecution conf story ["invent"]
  Dev -> do -- for ongoing dev
    let conf = Interaction.Conf { debug = True, seeTrace = True, seeStats = False }
    traceExecution conf story ["invent"] -- TODO: read from file


traceExecution :: Interaction.Conf -> Story -> [String] -> IO ()
traceExecution conf story inputs = do
  let maxSteps = 10000
  let e = Evaluation.theEffect
  let s :: State = initState story
  let i :: Inter = runEff maxSteps s e
  runInter conf inputs i

dumpObjects :: Story -> IO ()
dumpObjects story = do
  let maxSteps = 10000
  let e = Objects.dump
  let s :: State = initState story
  let i :: Inter = runEff maxSteps s e
  let conf = Interaction.Conf { debug = True, seeTrace = False, seeStats = False }
  runInter conf [] i
