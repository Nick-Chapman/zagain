module Top (main)  where

import Prelude hiding (Word)

import Control.Monad (ap,liftM)
import Data.Array (Array,(!),listArray)
import Data.Word (Word8,Word16)

import qualified Data.ByteString as BS (readFile,unpack)

type Byte = Word8
type Word = Word16
type Addr = Int

main :: IO ()
main = do
  --putStrLn "*zagain*"
  let filename = "story/zork1.88-840726.z3"
  story <- loadStory filename
  let rs = initRunState story
  let messages = runEffect rs theEffect
  mapM_ putStrLn messages

theEffect :: Eff ()
theEffect = do
  let _ = (decode,exec) -- TODO
  Trace "theEffect.."
  let a = 0x6 -- initial PC
  w <- readW a
  Trace (show w)
  Trace "theEffect..done"

decode :: Addr -> Eff Instruction
decode = undefined

exec :: Instruction -> Eff ()
exec = undefined

readW :: Addr -> Eff Word
readW a = do
  hi <- ReadB a
  lo <- ReadB (a+1)
  pure $ wordOfHiLo HiLo {hi,lo}

data Instruction

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  ReadB :: Addr -> Eff Byte
  Trace :: String -> Eff ()

runEffect :: RunState -> Eff () -> [String]
runEffect rs eff = loop eff $ \() -> []
  where
    loop :: Eff a -> (a -> [String]) -> [String]
    loop eff0 k = case eff0 of
      Ret a -> k a
      Bind e f -> loop e $ \a -> loop (f a) k
      ReadB a -> k (readB rs a)
      Trace mes -> mes : k ()

data RunState = RunState { story :: Story }

readB :: RunState -> Addr -> Byte
readB RunState{story} a = readStoryByte story a

initRunState :: Story -> RunState
initRunState story = RunState { story }

data Story = Story { size :: Int, bytesA :: Array Addr Byte }

readStoryByte :: Story -> Addr -> Byte
readStoryByte Story{size, bytesA} a =  if
  | a < size -> bytesA ! a
  | otherwise -> error (show ("readStoryByte",a,size))

loadStory :: FilePath -> IO Story
loadStory path = do
  bytes <- loadBytes path
  let size = length bytes
  let bytesA = listArray (0,size-1) bytes
  pure $ Story { size, bytesA }

loadBytes :: FilePath -> IO [Word8]
loadBytes path = BS.unpack <$> BS.readFile path

wordOfHiLo :: HiLo Byte -> Word
wordOfHiLo HiLo{hi,lo} = 256 * fromIntegral hi + fromIntegral lo

data HiLo a = HiLo { hi :: a, lo :: a }
