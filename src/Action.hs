
-- | The (inter)action of z-machine execution with input/output.
module Action (Conf(..),Action(..),Stats(..),runAction) where

import Control.Monad (when)
import Numbers (Addr)
import Operation (Operation)
import Text.Printf (printf)
import qualified Operation as Op (pretty)

data Conf = Conf
  { debug :: Bool
  , seeTrace :: Bool
  , seeStats :: Bool
  , mojo :: Bool
  , bufferOutput :: Bool
  }

--[interaction type]--------------------------------------------------

data Action
  = Trace String Stats Int Addr Operation Action
  | Output String Action
  | Debug String Action
  | Input Int (String -> Action)
  | Stop Int

--[run interaction as IO]---------------------------------------------

runAction :: Conf -> [String] -> Action -> IO ()
runAction Conf{seeStats,seeTrace,debug,mojo,bufferOutput} xs = loop 1 xs []
  where
    loop :: Int -> [String] -> [String] -> Action -> IO ()
    loop nInput xs buf = \case
      Trace stateString stats n a instruction next -> do
        when mojo $ do
          printf "%d %s\n" n stateString
        when seeTrace $ do
          let sd = if seeStats then show stats ++ " " else ""
          printf "%s(Decode %d %s %s)\n" sd n (show a) (Op.pretty instruction)
        loop nInput xs buf next
      Output text next -> do
        when (not bufferOutput) $ putStr text
        loop nInput xs (if bufferOutput then text:buf else buf) next
      Debug s next -> do
        when (debug) $ putStrLn ("Debug: " ++ s)
        loop nInput xs buf next
      Input count f -> do
        flushBuffer count buf
        case xs of
          [] -> do
            putStrLn "\n[no more input]"
            pure ()
          input:xs -> do
            let _ = when debug $ putStr (printf "[%d]" nInput) -- how far through the walkthrough we are?
            putStrLn input
            loop (nInput+1) xs [] (f input)
      Stop count -> do
        flushBuffer count buf

    flushBuffer :: Int -> [String] -> IO ()
    flushBuffer count buf = do
      when seeTrace $ do
        printf "\n[executed: %d instructions]\n" count
      mapM_ putStr (reverse buf)


--[static/dynamic GetByte stats]-------------------------------------

data Stats = Stats
  { ct :: Int -- story byte fetches (which dont depend on the state)
  , rt :: Int -- run-time story/override byte fetches
  }

instance Show Stats where
  show Stats{ct,rt} = printf "[%d/%d]" ct rt
