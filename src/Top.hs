
module Top (main)  where

import Prelude hiding (Word)

import System.Environment (getArgs)
import qualified Dis

main :: IO ()
main = getArgs >>= (run . parseCommandLine)

parseCommandLine :: [String] -> Config
parseCommandLine = \case
  [] -> DisReach
  args ->
    error (show ("parse",args))

data Config = DisReach

run :: Config -> IO ()
run = \case
  DisReach -> Dis.runReach
