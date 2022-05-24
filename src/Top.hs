
-- | Entry point for all z-machine code: Command like parsing and dispatch.
module Top (main)  where

import Action (Conf(..),runAction)
import Control.Monad (when)
import Dictionary (fetchDict)
import Dis (disassemble)
import Fetch (runFetch)
import Story (loadStory)
import System.Environment (getArgs)
import qualified Semantics (theEffect)
import qualified Interpreter (run)
import qualified Console

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

data Mode = Run | Dis | Dictionary | Interact

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
    , mojo = False
    , bufferOutput = True
    , wrap = Nothing
  }

parseCommandLine :: [String] -> IO Config
parseCommandLine = loop config0
  where
    loop c@Config{iconf,inputs} = \case
      [] -> pure c
      "console":more -> loop c { mode = Interact } more
      "dis":more -> loop c { mode = Dis } more
      "dict":more -> loop c { mode = Dictionary } more
      "-nodebug":more -> loop c { iconf = iconf { debug = False }} more
      "-trace":more -> loop c { iconf = iconf { seeTrace = True }} more
      "-mojo":more -> loop c { iconf = iconf { mojo = True }} more
      "-nobuf":more -> loop c { iconf = iconf { bufferOutput = False }} more
      "-wrap":i:more -> loop c { iconf = iconf { wrap = Just (read i) }} more
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
    Dictionary -> do
      story <- loadStory storyFile
      let (dict,_) = runFetch 0 story fetchDict
      print dict
    Run -> do
      let seed = 777
      story <- loadStory storyFile
      when trace $ putStrLn "[release/serial: 88/840726, z-version: .z3}"
      let a = Interpreter.run seed story Semantics.theEffect
      runAction iconf inputs a
    Interact -> do
      let seed = 777
      story <- loadStory storyFile
      when trace $ putStrLn "[release/serial: 88/840726, z-version: .z3}"
      let a = Interpreter.run seed story Semantics.theEffect
      Console.runAction a --iconf inputs
