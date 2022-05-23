
module Console (runAction) where

import Action (Action(..))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Text.Printf (printf)
import qualified System.Console.ANSI as AN
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL

runAction :: Action -> IO ()
runAction action = do
  HL.runInputT haskelineSettings $ do
    transcript <- lift $ readTranscript
    HL.putHistory transcript
    let lines = reverse (HL.historyLines transcript)
    replay lines action

-- replay the .transcript lines...
replay :: [String] -> Action -> HL.InputT IO ()
replay = loop 1
  where
    showOld = False
    loop :: Int -> [String] -> Action -> HL.InputT IO ()
    loop n = \case
      line:rest -> inner
        where
          inner = \case
            Stop{} -> do pure ()
            TraceInstruction _ _ _ _ next -> do inner next
            Debug _ next -> do inner next
            Output text next -> do
              when showOld $ (lift $ putStr (col AN.Cyan text))
              inner next
            Input _ _ f -> do
              lift $ printf "[%i] %s\n" n line
              loop (n+1) rest (f line)
      [] ->
        repl n -- then start the repl

repl :: Int -> Action -> HL.InputT IO ()
repl = loop []
  where
    doWrap = True
    loop :: [String] -> Int -> Action -> HL.InputT IO ()
    loop buf n = \case
      Stop{} -> do
        let text = concat (reverse buf)
        let wrap = if doWrap then lineWrap else \_ -> id
        lift $ putStr (col AN.Cyan (wrap 80 text))
      TraceInstruction _ _ _ _ next -> do loop buf n next
      Debug _ next -> do loop buf n next
      Output text next -> do loop (text:buf) n next
      Input status _count f -> do
        let (text,prompt) = splitFinalPrompt (concat (reverse buf))
        let wrap = if doWrap then lineWrap else \_ -> id
        lift $ putStr (col AN.Cyan (wrap 80 text))
        lift $ putStrLn (col AN.Magenta (makeStatusLine status))
        let _numInputs = col AN.Green $ printf "[%i]" n
        let xprompt = col AN.Cyan (prompt ++ " ")
        HL.getInputLine xprompt >>= \case
          Nothing -> pure () -- Ctr-D
          Just line -> do
            HL.modifyHistory (HL.addHistory line)
            HL.getHistory >>= lift . writeTranscript
            loop [] (n+1) (f line)

makeStatusLine :: (String,String) -> String
makeStatusLine (xs,ys) = do
  let max = 80
  let xs' = take 30 xs
  let ys' = take 30 ys
  let i = max - (length xs' + length ys' + 4)
  let bar = take i (repeat '-')
  "--" ++ xs' ++ bar ++ ys' ++ "--"

splitFinalPrompt :: String -> (String,String)
splitFinalPrompt s = do
  let (xs,ys) = span (not . (== '\n')) (reverse s)
  (reverse ys, reverse xs)

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


col :: AN.Color -> String -> String
col c s =
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid c] <> s <>
  AN.setSGRCode [AN.SetColor AN.Foreground AN.Vivid AN.White]


-- keep transcript in opposite order from HL standard (newest at end of file)

haskelineSettings :: HL.Settings IO
haskelineSettings =
  HL.defaultSettings {HL.autoAddHistory = False}

writeTranscript :: HL.History -> IO ()
writeTranscript =
  HL.writeHistory transcriptFile . revTranscript

readTranscript :: IO HL.History
readTranscript =
  fmap revTranscript $ HL.readHistory transcriptFile

revTranscript :: HL.History -> HL.History
revTranscript =
  foldl (flip HL.addHistory) HL.emptyHistory . HL.historyLines

transcriptFile :: FilePath
transcriptFile = ".transcript"
