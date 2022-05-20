
module Console (runAction) where

import Action (Action(..))
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
    loop :: Int -> [String] -> Action -> HL.InputT IO ()
    loop n = \case
      line:rest -> inner
        where
          inner = \case
            Stop{} -> do pure ()
            TraceInstruction _ _ _ _ _ next -> do inner next
            Debug _ next -> do inner next
            Output _ next -> do inner next
            Input _ f -> do
              lift $ printf "[%i] %s\n" n line
              loop (n+1) rest (f line)
      [] ->
        repl n -- then start the repl

repl :: Int -> Action -> HL.InputT IO ()
repl = loop []
  where
    loop :: [String] -> Int -> Action -> HL.InputT IO ()
    loop buf n = \case
      Stop{} -> do pure ()
      TraceInstruction _ _ _ _ _ next -> do loop buf n next
      Debug _ next -> do loop buf n next
      Output text next -> do loop (text:buf) n next
      Input count f -> do
        lift $ flushBuffer count buf
        HL.getInputLine (col AN.Green $ printf "[%i]" n) >>= \case
          Nothing -> pure () -- Ctr-D
          Just line -> do
            HL.modifyHistory (HL.addHistory line)
            HL.getHistory >>= lift . writeTranscript
            loop [] (n+1) (f line)

    flushBuffer :: Int -> [String] -> IO ()
    flushBuffer _count buf = do
      -- printf "\n[executed: %d instructions]\n" count
      mapM_ (putStr . col AN.Cyan) (reverse buf)


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
