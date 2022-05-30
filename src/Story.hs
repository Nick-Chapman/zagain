
-- | A z-machine story-file.
module Story (Story(header,size),loadStory,readStoryByte,OOB_Mode(..)) where

import Data.Array (Array,(!),listArray)
import Header (Header(..),Zversion(..))
import Numbers (Byte,Addr,makeHiLo)
import Text.Printf (printf)
import qualified Data.ByteString as BS (readFile,unpack)

data Story = Story
  { size :: Addr
  , bytesA :: Array Addr Byte
  , header :: Header
  }

loadStory :: FilePath -> IO Story
loadStory path = do
  bytes <- loadBytes path
  let size = fromIntegral $ length bytes
  let bytesA = listArray (0,size-1) bytes
  let story = Story { size, bytesA, header = readHeader story }
  pure story

loadBytes :: FilePath -> IO [Byte]
loadBytes path = (map fromIntegral . BS.unpack) <$> BS.readFile path

data OOB_Mode = OOB_Error String | OOB_Zero

readStoryByte :: OOB_Mode -> Story -> Addr -> Byte
readStoryByte mode Story{size, bytesA} a =  if
  | a < size && a >= 0 -> bytesA ! a
  | otherwise -> oob
    where
      oob =
        case mode of
          OOB_Zero -> 0
          OOB_Error who ->
            error (printf "readStoryByte(%s): %s, size: %s"
                   who (show a) (show size))

readHeader :: Story -> Header
readHeader story = Header
  { zv = versionOfByte $ getB 0x0
  , highMem = getA 0x4
  , initialPC = getA 0x6
  , dictionary = getA 0x8
  , objectTable = getA 0xA
  , globalVars = getA 0xC
  , staticMem = getA 0xE
  , abbrevTable = getA 0x18
  }
  where
    getA :: Addr -> Addr
    getA a = do
      let hi = getB a
      let lo = getB (a+1)
      fromIntegral $ makeHiLo hi lo

    getB :: Addr -> Byte
    getB a = readStoryByte (OOB_Error "readHeader") story a

versionOfByte :: Byte -> Zversion
versionOfByte = \case
  1 -> Z1
  2 -> Z2
  3 -> Z3
  4 -> undefined Z4
  5 -> undefined Z5
  6 -> undefined Z6
  n -> error (printf "unsupported z-machine version: %s" (show n))
