
module Interaction (Conf(..),Inter(..),Stats(..),runInter) where

import Control.Monad (when)
import Instruction (Instruction)
import Numbers (Addr)
import Text.Printf (printf)
import qualified Instruction as I (pretty)

data Conf = Conf
  { debug :: Bool
  , seeTrace :: Bool
  , seeStats :: Bool
  }

--[interaction type]--------------------------------------------------

data Inter
  = I_Trace Stats Int Addr Instruction Inter
  | I_Output String Inter
  | I_Debug String Inter
  | I_Input Int (String -> Inter)
  | I_Stop

--[run interaction as IO]---------------------------------------------

runInter :: Conf -> [String] -> Inter -> IO ()
runInter Conf{seeStats,seeTrace,debug} xs = loop xs []
  where
    loop :: [String] -> [String] -> Inter -> IO ()
    loop xs buf = \case
      I_Trace stats n a instruction next -> do
        when seeTrace $ do
          let sd = if seeStats then show stats ++ " " else ""
          printf "%s(Decode %d %s %s)\n" sd n (show a) (I.pretty instruction)
        loop xs buf next
      I_Output text next -> do
        --printf "OUTPUT:[%s]\n" text
        loop xs (text:buf) next
      I_Debug s next -> do
        when (debug) $ putStrLn ("Debug: " ++ s)
        loop xs buf next
      I_Input count f -> do
        when seeTrace $ do
          printf "\n[executed: %d instructions]\n" count
        mapM_ putStr (reverse buf)
        case xs of
          [] -> do
            putStrLn "\n[no more input]"
            pure ()
          input:xs -> do
            putStrLn input
            loop xs [] (f input)
      I_Stop -> do
        pure ()


--[static/dynamic GetByte stats]-------------------------------------

data Stats = Stats
  { ct :: Int -- story byte fetches (which dont depend on the state)
  , rt :: Int -- run-time story/override byte fetches
  }

instance Show Stats where
  show Stats{ct,rt} = printf "[%d/%d]" ct rt
