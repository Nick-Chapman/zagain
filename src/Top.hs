
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

  -- For regression, match the commad line of old niz
  -- (temp: with user commands also passed on command line)
  ["reg","-trace",_storyIgnored] -> RegTrace []
  ["reg","-trace",_storyIgnored,w1] -> RegTrace [w1]

  "reg":_storyIgnored:ws -> RegWalk ws
  "debug":_storyIgnored:ws -> DebugWalk ws

  "trace":ws -> Trace ws
  [] -> Latest
  ws -> Walk ws

  --args -> error (show ("parse",args))

data Config
  = Dis | Objects | Dictionary
  | RegTrace [String] -- regression traces
  | RegWalk [String]
  | DebugWalk [String]
  | Latest
  | Trace [String]
  | Walk [String]

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
    traceExecution conf story ws
      where
        conf = Interaction.Conf { debug = False
                                , seeTrace = True
                                , seeStats = False }
  RegWalk ws -> do
    traceExecution conf story ws
      where
        conf = Interaction.Conf { debug = False
                                , seeTrace = False
                                , seeStats = False }
  DebugWalk ws -> do
    traceExecution conf story ws
      where
        conf = Interaction.Conf { debug = True
                                , seeTrace = False
                                , seeStats = False }
  Latest -> do
    print "**latest dev**"
    traceExecution conf story ["open mailbox"]
      where
        conf = Interaction.Conf { debug = True
                                , seeTrace = True
                                , seeStats = False }
  Trace ws -> do
    putStrLn "[release/serial: 88/840726, z-version: .z3}" -- hack
    traceExecution conf story ws
      where
        conf = Interaction.Conf { debug = True
                                , seeTrace = True
                                , seeStats = False }
  Walk ws -> do
    traceExecution conf story ws
      where
        conf = Interaction.Conf { debug = True
                                , seeTrace = False
                                , seeStats = False }


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
  let conf = Interaction.Conf { debug = True, seeTrace = False, seeStats = False }
  runInter conf [] i
