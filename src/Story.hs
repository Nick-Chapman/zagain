
module Story (Story,loadStory,readStoryByte) where

import Data.Array (Array,(!),listArray)
import Data.Word (Word8)
import Numbers (Addr)
import qualified Data.ByteString as BS (readFile,unpack)

type Byte = Word8

data Story = Story { size :: Int, bytesA :: Array Int Byte }

loadStory :: FilePath -> IO Story
loadStory path = do
  bytes <- loadBytes path
  let size = length bytes
  let bytesA = listArray (0,size-1) bytes
  pure $ Story { size, bytesA }

loadBytes :: FilePath -> IO [Word8]
loadBytes path = BS.unpack <$> BS.readFile path

readStoryByte :: Story -> Addr -> Byte
readStoryByte Story{size, bytesA} a =  if
  | i < size -> bytesA ! i
  | otherwise -> error (show ("readStoryByte",a,size))
    where i = fromIntegral a
