
-- | Run the z-machine action with a canned walkthrough.
module WalkThrough (runAction) where

import Action (Conf(..),Action(..))
import Control.Monad (when)
import Text.Printf (printf)
import TextDisplay(lineWrap)

runAction :: Conf -> [String] -> Action -> IO ()
runAction Conf{seeTrace,debug,mojo,bufferOutput,wrap} xs = loop 1 xs []
  where
    wrapF = case wrap of Just w -> lineWrap w; Nothing -> id

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
            putStrLn "" -- extra blank line to match frotz
            loop (nInput+1) xs [] (f input)
      Stop count -> do
        flushBuffer count buf

    flushBuffer :: Int -> [String] -> IO ()
    flushBuffer count buf = do
      when seeTrace $ do
        printf "\n[executed: %d instructions]\n" count
      putStr (wrapF (concat (reverse buf)))
