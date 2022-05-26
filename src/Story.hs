
-- | A z-machine story-file.
module Story (Story(header,size),loadStory,readStoryByte,OOB_Mode(..)) where

import Data.Array (Array,(!),listArray)
import Header (Header(..),Zversion(..))
import Numbers (Byte,Addr)
import Text.Printf (printf)
import qualified Data.ByteString as BS (readFile,unpack)

data Story = Story
  { size :: Int
  , bytesA :: Array Int Byte
  , header :: Header
  }

loadStory :: FilePath -> IO Story
loadStory path = do
  bytes <- loadBytes path
  let size = length bytes
  let bytesA = listArray (0,size-1) bytes
  let story = Story { size, bytesA, header = readHeader story }
  pure story

loadBytes :: FilePath -> IO [Byte]
loadBytes path = (map fromIntegral . BS.unpack) <$> BS.readFile path

data OOB_Mode = OOB_Error | OOB_Zero

readStoryByte :: OOB_Mode -> Story -> Addr -> Byte
readStoryByte mode Story{size, bytesA} a =  if
  | i < size && i >= 0 -> bytesA ! i
  | otherwise ->  oob
    where
      i = fromIntegral a
      oob =
        case mode of
          OOB_Error -> error (show ("readStoryByte",a,size))
          OOB_Zero -> 0

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
      256 * fromIntegral hi + fromIntegral lo

    getB :: Addr -> Byte
    getB a = readStoryByte OOB_Error story a

versionOfByte :: Byte -> Zversion
versionOfByte = \case
  1 -> undefined Z1
  2 -> undefined Z2
  3 -> Z3
  4 -> undefined Z4
  5 -> undefined Z5
  6 -> undefined Z6
  n -> error (printf "unsupported z-machine version: %s" (show n))
