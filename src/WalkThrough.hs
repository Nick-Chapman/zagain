
-- | Run the z-machine action with a canned walkthrough.
module WalkThrough (runAction) where

import Action (Conf(..),Action(..))
import Control.Monad (when)
import Text.Printf (printf)
import TextDisplay(lineWrap)

runAction :: Conf -> [String] -> Action -> IO ()
runAction Conf{debug,seeTrace,mojo,showInput,bufferOutput,wrapSpec} xs = loop 1 xs []
  where
    wrap = case wrapSpec of Just w -> lineWrap w; Nothing -> id

    loop :: Int -> [String] -> [String] -> Action -> IO ()
    loop n xs buf = \case
      TraceInstruction stateString count a op next -> do
        when mojo $ do
          printf "%d %s\n" count stateString
        when seeTrace $ do
          printf "%d %s %s\n" count (show a) (show op)
        loop n xs buf next
      TraceRoutineCall _ next -> do loop n xs buf next
      TextStyle _sb next -> do -- ignore the text style
        loop n xs buf next
      Output text next -> do
        when (not bufferOutput) $ putStr text
        loop n xs (if bufferOutput then text:buf else buf) next
      Debug msg next -> do
        when (debug) $ putStrLn ("Debug: " ++ msg)
        loop n xs buf next
      Input _ count f -> do -- ignore the status line
        flushBuffer count buf
        case xs of
          [] -> do
            --printf "[no more input]\n"
            pure ()
          input:xs -> do
            when showInput $ do
              printf "[%d]%s\n\n\n" n input
            loop (n+1) xs [] (f input)
      Stop count -> do
        flushBuffer count buf
        --printf "[the game ended]\n"
        pure ()

    flushBuffer :: Int -> [String] -> IO ()
    flushBuffer count buf = do
      let text = concat (reverse buf)
      when seeTrace $ do
        printf "\n[executed: %d instructions]\n" count
      putStr (wrap text)
