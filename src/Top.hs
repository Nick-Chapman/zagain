
module Top (main)  where

import Prelude hiding (Word)

import System.Environment (getArgs)
import qualified Dis

main :: IO ()
main = getArgs >>= (run . parseCommandLine)

parseCommandLine :: [String] -> Config
parseCommandLine = \case
  ["disAll"] -> DisAll
  [] -> DisReach
  args ->
    error (show ("parse",args))

data Config = DisAll | DisReach

run :: Config -> IO ()
run = \case
  DisAll -> Dis.runAll
  DisReach -> Dis.runReach
