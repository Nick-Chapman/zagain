
-- | The (inter)action of z-machine execution with input/output.
module Action (Conf(..),Action(..),runAction,lineWrap) where

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
    doWrap = False
    wrap = if doWrap then lineWrap 80 else id

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
            --putStrLn "" -- TODO: match frotz
            loop (nInput+1) xs [] (f input)
      Stop count -> do
        flushBuffer count buf

    flushBuffer :: Int -> [String] -> IO ()
    flushBuffer count buf = do
      when seeTrace $ do
        printf "\n[executed: %d instructions]\n" count
      putStr (wrap (concat (reverse buf)))


lineWrap :: Int -> String -> String
lineWrap max text =
  unlines [ lineWrapOneLine line | line <- lines text ]
  where
    lineWrapOneLine line = do
      let (wsIndent,remainder) = span (== ' ') line
      case words remainder of
        [] -> []
        w1:ws -> do
          let indentedW1 = wsIndent ++ w1
          loop [indentedW1] (length indentedW1) ws
          where
            loop :: [String] -> Int-> [String] -> String
            loop acc pos = \case
              [] -> concat (reverse acc)
              w:ws -> do
                let indentedW = wsIndent ++ w
                let posW = pos + length w + 1
                let makeSplit = posW > max
                let pos' = if makeSplit then length indentedW else posW
                let sep = if makeSplit then "\n"++wsIndent else " "
                loop (w : sep : acc) pos' ws
