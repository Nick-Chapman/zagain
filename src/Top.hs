
module Top (main)  where

import Dis (disZork)
import Prelude hiding (Word)
import System.Environment (getArgs)
import Walk (walkZork)

main :: IO ()
main = getArgs >>= (run . parseCommandLine)

parseCommandLine :: [String] -> Config
parseCommandLine = \case
  ["dis-zork"] -> DisZork
  [] -> WalkZork
  args ->
    error (show ("parse",args))

data Config = DisZork | WalkZork

run :: Config -> IO ()
run = \case
  DisZork -> disZork
  WalkZork -> walkZork
