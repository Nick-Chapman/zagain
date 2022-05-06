
module Top (main)  where

import Prelude hiding (Word)

import System.Environment (getArgs)
import qualified Dis (run)

main :: IO ()
main = getArgs >>= (run . parseCommandLine)

parseCommandLine :: [String] -> Config
parseCommandLine = \case
  ["dis"] -> Dis1
  [] -> DisMore
  args ->
    error (show ("parse",args))

data Config = Dis1 | DisMore

run :: Config -> IO ()
run = \case
  Dis1 -> Dis.run
  DisMore -> Dis.run
