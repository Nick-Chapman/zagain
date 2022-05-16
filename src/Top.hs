
-- | Entry point for all z-machine code: Command like parsing and dispatch.
module Top (main)  where

import Action (Conf(..),runAction)
import Control.Monad (when)
import Dictionary (fetchDict)
import Dis (disassemble)
import Fetch (runFetch)
import Story (loadStory)
import System.Environment (getArgs)
import Walk (initState,runEff)
import qualified Objects (dump)
import qualified Semantics (theEffect)

main :: IO ()
main = do
  args <- getArgs
  config <- parseCommandLine args
  run config

data Config = Config
  { mode :: Mode
  , storyFile :: FilePath
  , iconf :: Action.Conf
  , inputs :: [String]
  }

data Mode = Run | Dis | Objects | Dictionary

config0 :: Config
config0 = Config
  { mode = Run
  , storyFile = "story/zork1.88-840726.z3"
  , iconf = iconf0
  , inputs = []
  } where
  iconf0 = Action.Conf
    { debug = True
    , seeTrace = False
    , seeStats = False
    , mojo = False
    , bufferOutput = True
  }

parseCommandLine :: [String] -> IO Config
parseCommandLine = loop config0
  where
    loop c@Config{iconf,inputs} = \case
      [] -> pure c

      "dis":more -> loop c { mode = Dis } more
      "objects":more -> loop c { mode = Objects } more
      "dict":more -> loop c { mode = Dictionary } more

      "-nodebug":more -> loop c { iconf = iconf { debug = False }} more
      "-trace":more -> loop c { iconf = iconf { seeTrace = True }} more
      "-stats":more -> loop c { iconf = iconf { seeStats = True }} more
      "-mojo":more -> loop c { iconf = iconf { mojo = True }} more
      "-nobuf":more -> loop c { iconf = iconf { bufferOutput = False }} more

      "-type":line:more -> loop c { inputs = inputs ++ [line] } more

      "-walk":path:more -> do
        xs <- Prelude.lines <$> readFile path
        loop c { inputs = inputs ++ xs } more

      storyFile:more ->
        loop c { storyFile } more


run :: Config -> IO ()
run Config{mode,storyFile,iconf=iconf@Conf{seeTrace=trace},inputs} = do
  case mode of
    Dis -> do
      story <- loadStory storyFile
      disassemble story
    Objects -> do
      story <- loadStory storyFile
      let maxSteps = 100000
      let i = runEff maxSteps (initState story) Objects.dump
      runAction iconf [] i
    Dictionary -> do
      story <- loadStory storyFile
      let (dict,_,_) = runFetch 0 story fetchDict
      print dict
    Run -> do
      story <- loadStory storyFile
      when trace $ putStrLn "[release/serial: 88/840726, z-version: .z3}"
      let maxSteps = 100000
      let i = runEff maxSteps (initState story) Semantics.theEffect
      runAction iconf inputs i
