
module Console (runAction) where

import Action (Action(..),Conf(..),StatusLine(..))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Set (Set)
import Numbers (Style(..))
import Text.Printf (printf)
import TextDisplay(lineWrap)
import qualified Data.Set as Set
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.History as HL

import System.Console.ANSI
  ( Color(Cyan,Red,Magenta)
  , SGR(SetColor,Reset)
  , ConsoleLayer(Foreground)
  , ColorIntensity(Vivid)
  , ConsoleIntensity(BoldIntensity)
  , SGR(SetConsoleIntensity,SetSwapForegroundBackground,SetItalicized)
  , ConsoleIntensity(NormalIntensity)
  , setSGRCode
  )

runAction :: Conf -> Action -> IO ()
runAction conf action = do
  HL.runInputT haskelineSettings $ do
    transcript <- lift $ readTranscript
    HL.putHistory transcript
    let lines = reverse (HL.historyLines transcript)
    replay conf lines action

-- replay the .transcript lines...
replay :: Conf -> [String] -> Action -> HL.InputT IO ()
replay conf@Conf{debug=_debug} = loop 1
  where
    showOld = False
    loop :: Int -> [String] -> Action -> HL.InputT IO ()
    loop n = \case
      line:rest -> inner
        where
          inner = \case
            Tab a -> do inner a
            UnTab a -> do inner a
            Stop{} -> do pure ()
            TraceInstruction _ _ _ _ next -> do inner next
            TraceRoutineCall _ next -> do inner next
            Debug _msg next -> do
              --when (_debug) $ lift $ putStrLn ("Debug: " ++ _msg)
              inner next
            TextStyle _sb next ->
              inner next
            Output text next -> do
              when showOld $ (lift $ putStr text)
              inner next
            Input _ _ f -> do
              lift $ printf "[%i] %s\n" n line
              loop (n+1) rest (f line)
      [] ->
        repl conf -- then start the repl

repl :: Conf -> Action -> HL.InputT IO ()
repl Conf{debug,seeTrace,mojo,bufferOutput,wrapSpec} = loop styles0 []
  where
    wrap = case wrapSpec of Just w -> lineWrap w; Nothing -> id
    loop :: Styles -> [String] -> Action -> HL.InputT IO ()
    loop styles buf = \case
      Tab a -> do loop styles buf a
      UnTab a -> do loop styles buf a
      Stop{} -> do
        let text = concat (reverse buf)
        lift $ put gameCol (wrap text)
      TraceInstruction stateString count a op next -> do
        when mojo $ do
          lift $ printf "%d %s\n" count stateString
        when seeTrace $ do
          lift $ printf "%d %s %s\n" count (show a) (show op)
        loop styles buf next
      TraceRoutineCall _ next -> do loop styles buf next
      Debug msg next -> do
        when (debug) $ lift $ put debugCol ("Debug: " ++ msg ++ "\n")
        loop styles buf next
      TextStyle sb next -> do
        let (styles',note) = changeStyle sb styles
        loop styles' buf (Output note next)
      Output text next -> do
        when (not bufferOutput) $ do
          lift $ put gameCol text
        loop styles (if bufferOutput then text:buf else buf) next
      Input statusLineM _count f -> do
        let (text,prompt) = splitFinalPrompt (concat (reverse buf))
        lift $ put gameCol (wrap text)
        case statusLineM of
          Nothing -> pure ()
          Just statusLine ->
            lift $ put statusLineCol (makeStatusLine statusLine ++ "\n")
        let xprompt = wrapCol gameCol (prompt ++ " ")
        HL.getInputLine xprompt >>= \case
          Nothing -> pure () -- Ctr-D
          Just line -> do
            HL.modifyHistory (HL.addHistory line)
            HL.getHistory >>= lift . writeTranscript
            loop styles [] (f line)

    gameCol = Cyan
    debugCol = Red
    statusLineCol = Magenta

data Styles = Styles { set :: Set Style }

styles0 :: Styles
styles0 = Styles Set.empty

changeStyle :: (Style,Bool) -> Styles -> (Styles,String)
changeStyle (s,goal) current@Styles{set} = do
  case (s `Set.member` set, goal) of
    (False,False) -> (current,"")
    (False,True) -> (current { set = Set.insert s set },styleOn s)
    (True,False) -> (current { set = Set.delete s set },styleOff s)
    (True,True) -> (current,"")

styleOn,styleOff :: Style -> String

styleOn = \case
  Reverse -> setSGRCode [SetSwapForegroundBackground True]
  Fixed -> "{fixed}" -- we are already using a fixed with font
  Italic -> setSGRCode [SetItalicized True]
  Bold -> setSGRCode [SetConsoleIntensity BoldIntensity]

styleOff = \case
  Reverse -> setSGRCode [SetSwapForegroundBackground False]
  Fixed -> "{fixed-off}" -- we are already using a fixed with font
  Italic -> setSGRCode [SetItalicized False]
  Bold -> setSGRCode [SetConsoleIntensity NormalIntensity]

put :: Color -> String -> IO ()
put col str = putStr (wrapCol col str)

wrapCol :: Color -> String -> String
wrapCol c s =
  setSGRCode [SetColor Foreground Vivid c] <> s <>
  setSGRCode [Reset] --SetColor Foreground Vivid White]

makeStatusLine :: StatusLine -> String
makeStatusLine StatusLine{left=xs,right=ys} = do
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
