module Top (main)  where

import Prelude hiding (Word)

import Control.Monad (ap,liftM)
import Data.Array (Array,(!),listArray)
import Data.Word (Word8,Word16)
import qualified Data.ByteString as BS (readFile,unpack)
import Decode (decodeInstruction)

type Byte = Word8
type Word = Word16
type Addr = Int

main :: IO ()
main = do
  let filename = "story/zork1.88-840726.z3"
  story <- loadStory filename
  let rs = initRunState story
  let messages = runEffect rs theEffect
  mapM_ putStrLn messages

theEffect :: Eff ()
theEffect = do
  Trace "theEffect.."
  let a = 0x6 -- initial PC
  w <- readW a
  Trace (show w)
  disFrom (addrOfWord w)

disFrom :: Addr -> Eff ()
disFrom a = do
  bs <- ReadBs a
  let (i,n) = decodeInstruction a bs
  Trace (show (a,i))
  disFrom (a+n)

addrOfWord :: Word -> Addr
addrOfWord = fromIntegral

readW :: Addr -> Eff Word
readW a = do
  hi <- ReadB a
  lo <- ReadB (a+1)
  pure $ wordOfHiLo HiLo {hi,lo}

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  ReadB :: Addr -> Eff Byte
  Trace :: String -> Eff ()
  ReadBs :: Addr -> Eff [Byte]

runEffect :: RunState -> Eff () -> [String]
runEffect rs eff = loop eff $ \() -> []
  where
    loop :: Eff a -> (a -> [String]) -> [String]
    loop eff0 k = case eff0 of
      Ret a -> k a
      Bind e f -> loop e $ \a -> loop (f a) k
      ReadB a -> k (readB rs a)
      ReadBs a -> k [ readB rs (a+i) | i <- [0..] ]
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
