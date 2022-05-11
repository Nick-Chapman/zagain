
module Top (main)  where

import Dis (disZork)
import System.Environment (getArgs)
import Walk (walkZork,dumpZorkObjects)

main :: IO ()
main = getArgs >>= (run . parseCommandLine)

parseCommandLine :: [String] -> Config
parseCommandLine = \case
  ["dis"] -> Dis
  ["objects"] -> Objects
  ["trace"] -> Trace
  [] -> Dev
  args -> error (show ("parse",args))

data Config = Dis | Objects | Trace | Dev

run :: Config -> IO ()
run = \case
  Dis -> disZork
  Objects -> dumpZorkObjects
  Trace -> walkZork
  Dev -> walkZork
