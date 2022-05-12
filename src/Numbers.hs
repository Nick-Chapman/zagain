
module Numbers
  ( Byte, byteOfValue
  , Addr, addrOfPackedWord
  , Value
  ) where

import Data.Bits (Bits)
import Data.Int (Int16)
import Data.Word (Word8,Word16)
import GHC.Ix (Ix)
import Text.Printf (printf)

matchNizFormat :: Bool
matchNizFormat = True -- need True for regression pass -- TODO: can this be dynamic?

newtype Byte = EightBits Word8
  deriving (Ord,Eq,Integral,Real,Enum,Num,Bits)

instance Show Byte where
  show (EightBits w8) = if
    | matchNizFormat -> show w8
    | otherwise -> printf "0x%02x" w8

byteOfValue :: Value -> Byte
byteOfValue v = do
  if v < 0 || v > 255 then error (show ("byteOfValue",v)) else fromIntegral v

newtype Addr = StoryIndex Int
  deriving (Ord,Eq,Num,Integral,Real,Enum)

addrOfPackedWord :: Value -> Addr
addrOfPackedWord w = StoryIndex (2 * fromIntegral w)

instance Show Addr where
  show (StoryIndex i) = if
    | matchNizFormat -> printf "%05i" i
    | otherwise -> printf "[%05i]" i

newtype Value = Value Int16 -- 16 bit signed values used for z-machine computations
  deriving (Ord,Eq,Integral,Real,Enum,Num,Bits,Ix)

instance Show Value where
  show (Value x) = if
    | matchNizFormat -> show (fromIntegral x :: Word16) -- show unsigned
    | otherwise -> show x
