
-- | A z-machine story-file.
module Story (Story(header,size),loadStory,readStoryByte) where

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

readStoryByte :: Story -> Addr -> Byte
readStoryByte Story{size, bytesA} a =  if
  | i < size -> bytesA ! i
  | otherwise -> 0xff --error (show ("readStoryByte",a,size)) -- TODO ??
    where i = fromIntegral a

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
    getB a = readStoryByte story a

versionOfByte :: Byte -> Zversion
versionOfByte = \case
  1 -> Z1
  2 -> Z2
  3 -> Z3
  4 -> Z4
  5 -> Z5
  n -> error (printf "unsupported z-machine version: %s" (show n))
