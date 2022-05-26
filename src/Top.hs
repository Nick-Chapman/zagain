
-- | Entry point for all z-machine code: Command like parsing and dispatch.
module Top (main)  where

import Action (Conf(..))
import Control.Monad (when)
import Dictionary (fetchDict)
import Dis (disassemble)
import Fetch (runFetch)
import Story (loadStory,OOB_Mode(..))
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Console (runAction)
import qualified Interpreter (runEffect)
import qualified Semantics (theEffect)
import qualified Static (explore)
import qualified WalkThrough (runAction)

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
  , mayStartConsole :: Bool
  }

data Mode = Dis | Dictionary | Run | Static

config0 :: Config
config0 = Config
  { mode = Run
  , storyFile = "story/zork1.88-840726.z3"
  , iconf = iconf0
  , inputs = []
  , mayStartConsole = True
  } where
  iconf0 = Action.Conf
    { debug = True
    , seeTrace = False
    , mojo = False
    , showInput = True
    , bufferOutput = True
    , wrapSpec = Nothing
  }

parseCommandLine :: [String] -> IO Config
parseCommandLine = loop config0
  where
    loop c@Config{iconf,inputs} = \case
      [] -> pure c
      "dis":more -> loop c { mode = Dis } more
      "dict":more -> loop c { mode = Dictionary } more
      "static":more -> loop c { mode = Static } more
      "-noconsole":more -> loop c { mayStartConsole = False } more
      "-nodebug":more -> loop c { iconf = iconf { debug = False }} more
      "-trace":more -> loop c { iconf = iconf { seeTrace = True }} more
      "-mojo":more -> loop c { iconf = iconf { mojo = True }} more
      "-nobuf":more -> loop c { iconf = iconf { bufferOutput = False }} more
      "-noinp":more -> loop c { iconf = iconf { showInput = False }} more
      "-wrap":i:more -> loop c { iconf = iconf { wrapSpec = Just (read i) }} more
      "-type":line:more -> loop c { inputs = inputs ++ [line] } more
      "-walk":path:more -> do
        xs <- Prelude.lines <$> readFile path
        loop c { inputs = inputs ++ xs } more
      storyFile:more ->
        loop c { storyFile } more

run :: Config -> IO ()
run Config{mode,storyFile,iconf=iconf@Conf{seeTrace=trace},inputs,mayStartConsole} = do
  case mode of
    Dis -> do
      story <- loadStory storyFile
      disassemble story
    Dictionary -> do
      story <- loadStory storyFile
      let (dict,_) = runFetch OOB_Error 0 story fetchDict
      print dict
    Run -> do
      let seed = 888
      story <- loadStory storyFile
      when trace $ putStrLn "[release/serial: 88/840726, z-version: .z3}"
      printf "\n\n"
      let a = Interpreter.runEffect seed story Semantics.theEffect
      case inputs of
        [] | mayStartConsole -> Console.runAction iconf a
        _ -> WalkThrough.runAction iconf inputs a
    Static -> do
      story <- loadStory storyFile
      Static.explore story
      pure ()
