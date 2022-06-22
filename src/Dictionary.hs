
-- | Decode the z-machine dictionary contained in a story-file.
module Dictionary (Dict(..),fetchDict) where

import Decode (ztext)
import Fetch (Fetch(..))
import Header (Header(..))
import Numbers (Byte,Addr,Value,makeHiLo,Zversion)
import qualified Data.Char as Char (chr)

data Dict = Dict
  { zv :: Zversion
  , base :: Addr
  , seps :: String
  , entryLength :: Int
  , numEntries :: Int
  , strings :: [String]
  }
  deriving Show

fetchDict :: Fetch Dict
fetchDict = do
  Header{zv,dictionary=base} <- StoryHeader
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
    pure $ Dict {zv,base,seps,entryLength,numEntries,strings}

fetchWord :: Fetch Value
fetchWord = do
  hi <- NextByte
  lo <- NextByte
  pure $ makeHiLo hi lo

charOfByte :: Byte -> Char
charOfByte b = Char.chr (fromIntegral b)
