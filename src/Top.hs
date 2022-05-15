
module Top (main)  where

import Dictionary (fetchDict)
import Dis (disassemble)
import Fetch (runFetch)
import Interaction (Inter,runInter)
import Story (Story,loadStory)
import System.Environment (getArgs)
import Walk (State,initState,runEff)
import qualified Evaluation (theEffect)
import Interaction (Conf(..))
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

  -- For regression, match the commad line of old niz
  -- (temp: with user commands also passed on command line)
  ["reg","-trace",_storyIgnored] -> RegTrace []
  ["reg","-trace",_storyIgnored,w1] -> RegTrace [w1]

  "reg":_storyIgnored:ws -> RegWalk ws
  "debug":_storyIgnored:ws -> DebugWalk ws

  "trace":ws -> Trace ws
  [] -> CompareWithMojo
  ws -> Walk ws

  --args -> error (show ("parse",args))

data Config
  = Dis | Objects | Dictionary
  | RegTrace [String] -- regression traces
  | RegWalk [String]
  | DebugWalk [String]
  | Trace [String]
  | Walk [String]
  | CompareWithMojo

run :: Story -> Config -> IO ()
run story = \case
  Dis ->
    disassemble story
  Objects ->
    dumpObjects story
  Dictionary -> do
    let (dict,_,_) = runFetch 0 story fetchDict
    print dict

  RegTrace ws -> do
    putStrLn "[release/serial: 88/840726, z-version: .z3}" -- hack
    traceExecution confD { debug = False, seeTrace = True } story ws
  RegWalk ws -> do
    traceExecution confD { debug = False } story ws
  DebugWalk ws -> do
    traceExecution confD story ws
  Trace ws -> do
    putStrLn "[release/serial: 88/840726, z-version: .z3}" -- hack
    traceExecution confD { seeTrace = True } story ws
  Walk ws -> do
    traceExecution confD story ws

  CompareWithMojo -> do
    print "**for comp with mojo**"
    traceExecution confD { mojo = True, bufferOutput = False } story
      ["open mailbox", "read leaflet"]

traceExecution :: Interaction.Conf -> Story -> [String] -> IO ()
traceExecution conf story inputs = do
  let maxSteps = 100000
  let e = Evaluation.theEffect
  let s :: State = initState story
  let i :: Inter = runEff maxSteps s e
  runInter conf inputs i

dumpObjects :: Story -> IO ()
dumpObjects story = do
  let maxSteps = 100000
  let e = Objects.dump
  let s :: State = initState story
  let i :: Inter = runEff maxSteps s e
  runInter confD [] i

confD :: Interaction.Conf
confD = Interaction.Conf
  { debug = True
  , seeTrace = False
  , seeStats = False
  , mojo = False
  , bufferOutput = True
  }
