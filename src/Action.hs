
-- | The (inter)action of z-machine execution with input/output.
module Action (Conf(..),Action(..),runAction) where

import Control.Monad (when)
import Numbers (Addr)
import Operation (Operation)
import Text.Printf (printf)

data Conf = Conf
  { debug :: Bool
  , seeTrace :: Bool
  , mojo :: Bool
  , bufferOutput :: Bool
  }

--[interaction type]--------------------------------------------------

data Action
  = TraceInstruction String Int Addr Operation Action
  | Output String Action
  | Debug String Action
  | Input (String,String) Int (String -> Action)
  | Stop Int

--[run interaction as IO]---------------------------------------------

runAction :: Conf -> [String] -> Action -> IO ()
runAction Conf{seeTrace,debug,mojo,bufferOutput} xs = loop 1 xs []
  where
    loop :: Int -> [String] -> [String] -> Action -> IO ()
    loop nInput xs buf = \case
      TraceInstruction stateString n a op next -> do
        when mojo $ do
          printf "%d %s\n" n stateString
        when seeTrace $ do
          printf "%d %s %s\n" n (show a) (show op)
        loop nInput xs buf next
      Output text next -> do
        when (not bufferOutput) $ putStr text
        loop nInput xs (if bufferOutput then text:buf else buf) next
      Debug s next -> do
        when (debug) $ putStrLn ("Debug: " ++ s)
        loop nInput xs buf next
      Input _ count f -> do -- TODO: show the status line?
        flushBuffer count buf
        case xs of
          [] -> do
            putStrLn "\n[no more input]"
            pure ()
          input:xs -> do
            putStr (printf "[%d]" nInput)
            putStrLn input
            loop (nInput+1) xs [] (f input)
      Stop count -> do
        flushBuffer count buf

    flushBuffer :: Int -> [String] -> IO ()
    flushBuffer count buf = do
      when seeTrace $ do
        printf "\n[executed: %d instructions]\n" count
      mapM_ putStr (reverse buf)
