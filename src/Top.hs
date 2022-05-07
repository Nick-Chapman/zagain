
module Top (main)  where

import Prelude hiding (Word)

import System.Environment (getArgs)
import Dis (disZork)

main :: IO ()
main = getArgs >>= (run . parseCommandLine)

parseCommandLine :: [String] -> Config
parseCommandLine = \case
  ["dis-zork"] -> DisZork
  args ->
    error (show ("parse",args))

data Config = DisZork

run :: Config -> IO ()
run = \case
  DisZork -> disZork
