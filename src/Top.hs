
-- | Entry point for all z-machine code: Command like parsing and dispatch.
module Top (main)  where

import Action (Conf(..))
import Control.Monad (when)
import Dictionary (fetchDict)
import Disassemble (disassemble)
import Fetch (runFetch)
import Story (loadStory,OOB_Mode(..))
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Compiler (compileEffect,runCode)
import qualified Console (runAction)
import qualified Interpreter (runEffect)
import qualified Semantics (smallStep)
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
  , viaCompiler :: Bool
  }

data Mode = Dictionary | Run | Disassemble

config0 :: Config
config0 = Config
  { mode = Run
  , storyFile = "story/zork1.88-840726.z3"
  , iconf = iconf0
  , inputs = []
  , mayStartConsole = True
  , viaCompiler = False
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
      "dict":more -> loop c { mode = Dictionary } more
      "dis":more -> loop c { mode = Disassemble } more
      "-noconsole":more -> loop c { mayStartConsole = False } more
      "-nodebug":more -> loop c { iconf = iconf { debug = False }} more
      "-comp":more -> loop c { viaCompiler = True } more
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
run Config{mode,storyFile,iconf=iconf@Conf{seeTrace=trace},inputs,mayStartConsole,viaCompiler} = do
  case mode of
    Dictionary -> do
      story <- loadStory storyFile
      let (dict,_) = runFetch (OOB_Error "Top.Dictionary") 0 story fetchDict
      print dict
    Run -> do
      let seed = 888
      story <- loadStory storyFile
      a <- if | not viaCompiler -> pure $ Interpreter.runEffect seed story Semantics.smallStep
              | otherwise -> do
                  code <- Compiler.compileEffect story Semantics.smallStep
                  pure $ Compiler.runCode seed code
      when trace $ putStrLn "[release/serial: 88/840726, z-version: .z3}"
      printf "\n\n"
      case inputs of
        [] | mayStartConsole -> Console.runAction iconf a
        _ -> WalkThrough.runAction iconf inputs a
    Disassemble -> do
      story <- loadStory storyFile
      disassemble story inputs
      pure ()
