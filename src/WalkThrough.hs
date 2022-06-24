
-- | Run the z-machine action with a canned walkthrough.
module WalkThrough (runAction) where

import Action (Conf(..),Action(..))
import Control.Monad (when)
import Operation (showOp)
import Text.Printf (printf)
import TextDisplay (lineWrap)

runAction :: Conf -> [String] -> Action -> IO ()
runAction Conf{debug,seeTrace,niz,frotz,mojo,showInput,bufferOutput,wrapSpec} xs = loop 0 1 xs []
  where
    indenting = False

    wrap = case wrapSpec of Just w -> lineWrap w; Nothing -> id

    loop :: Int -> Int -> [String] -> [String] -> Action -> IO ()
    loop i n xs buf = \case
      Tab next -> loop (i+1) n xs buf next
      UnTab next -> loop (i-1) n xs buf next
      TraceInstruction stateString count a op next -> do
        when niz $ do
          tab $ printf "(Decode %d %s %s)\n" count (show a) (showOp op)
        when frotz $ do
          tab $ printf "%8d : %s\n" count (show a)
        when mojo $ do
          tab $ printf "%d %s\n" count stateString
        when seeTrace $ do
          tab $ printf "%d %s %s\n" count (show a) (show op)
        loop i n xs buf next
      TraceRoutineCall _ next -> do loop i n xs buf next
      TextStyle _sb next -> do -- ignore the text style
        loop i n xs buf next
      Output text next -> do
        when (not bufferOutput) $ putStr text
        loop i n xs (if bufferOutput then text:buf else buf) next
      Debug msg next -> do
        when (debug) $ putStrLn ("Debug: " ++ msg)
        loop i n xs buf next
      Input _ count f -> do -- ignore the status line
        flushBuffer count buf
        case xs of
          [] -> do
            --printf "[no more input]\n"
            pure ()
          input:xs -> do
            when showInput $ do
              printf "[%d]%s\n\n\n" n input
            loop i (n+1) xs [] (f input)
      Stop count -> do
        flushBuffer count buf
        --printf "[the game ended]\n"
        pure ()

      where
        tab s = putStr $ if
          | indenting -> take (2*i) (repeat ' ') ++ s
          | otherwise -> s


    flushBuffer :: Int -> [String] -> IO ()
    flushBuffer count buf = do
      let text = concat (reverse buf)
      when seeTrace $ do
        printf "\n[executed: %d instructions]\n" count
      putStr (wrap text)
