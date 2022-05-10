
module Top (main)  where

import Dis (disZork)
import System.Environment (getArgs)
import Walk (walkZork,dumpZorkObjects)

main :: IO ()
main = getArgs >>= (run . parseCommandLine)

parseCommandLine :: [String] -> Config
parseCommandLine = \case
  ["dis-zork"] -> DisZork
  ["dump"] -> DumpZorkObjects
  [] -> WalkZork
  args ->
    error (show ("parse",args))

data Config = DisZork | WalkZork | DumpZorkObjects

run :: Config -> IO ()
run = \case
  DisZork -> disZork
  WalkZork -> walkZork
  DumpZorkObjects -> dumpZorkObjects
