
-- | Decode the z-machine dictionary contained in a story-file.
module Dictionary (Dict(..),fetchDict) where

import Decode (ztext)
import Fetch (Fetch(..))
import Numbers (Byte,Addr,Value)
import qualified Data.Char as Char (chr)

data Dict = Dict
  { seps :: String
  , entryLength :: Int
  , numEntries :: Int
  , strings :: [String]
  }
  deriving Show

fetchDict :: Fetch Dict
fetchDict = do
  base <- dictBase
  WithPC base $ do
    n <- fromIntegral <$> NextByte
    bs <- sequence $ take n (repeat NextByte)
    let seps = map charOfByte bs
    entryLength <- fromIntegral <$> NextByte
    numEntries <- fromIntegral <$> fetchWord
    strings <- sequence [ WithPC a ztext
                      | i <- [1::Int .. numEntries]
                      , let a :: Addr = base + fromIntegral (n + 4 + entryLength * (i-1))
                      ]
    pure $ Dict {seps,entryLength,numEntries,strings}

fetchWord :: Fetch Value
fetchWord = do
  hi <- NextByte
  lo <- NextByte
  pure (256 * fromIntegral hi + fromIntegral lo)

charOfByte :: Byte -> Char
charOfByte b = Char.chr (fromIntegral b)

dictBase :: Fetch Addr
dictBase = getAddress 0x8 -- TODO: share via header

getAddress :: Addr -> Fetch Addr
getAddress a = do
  hi <- getByte a
  lo <- getByte (a+1)
  pure (256 * fromIntegral hi + fromIntegral lo)

getByte :: Addr -> Fetch Byte
getByte a = WithPC a NextByte
