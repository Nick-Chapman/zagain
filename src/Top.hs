
module Top (main)  where

import Prelude hiding (Word)

import System.Environment (getArgs)
import qualified Dis (run,run1)

main :: IO ()
main = getArgs >>= (run . parseCommandLine)

parseCommandLine :: [String] -> Config
parseCommandLine = \case
  ["dis1"] -> Dis1
  [] -> DisMore
  args ->
    error (show ("parse",args))

data Config = Dis1 | DisMore

run :: Config -> IO ()
run = \case
  Dis1 -> Dis.run1
  DisMore -> Dis.run
