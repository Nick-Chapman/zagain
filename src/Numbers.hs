
-- | Various types of numbers used by the z-machine.
module Numbers
  ( Byte, byteOfValue
  , Addr, addrOfPackedWord
  , Value
  ) where

import Data.Bits (Bits)
import Data.Int (Int16)
import Data.Word (Word8)
import GHC.Ix (Ix)
import Text.Printf (printf)

newtype Byte = EightBits Word8
  deriving (Ord,Eq,Integral,Real,Enum,Num,Bits)

instance Show Byte where
  show (EightBits w8) = printf "0x%02x" w8

byteOfValue :: Value -> Byte
byteOfValue v = do
  if v < 0 || v > 255 then error (show ("byteOfValue",v)) else fromIntegral v

newtype Addr = StoryIndex Word
  deriving (Ord,Eq,Num,Integral,Real,Enum)

addrOfPackedWord :: Value -> Addr
addrOfPackedWord v = if
  | v < 0 -> StoryIndex (2 * (0x10000 + fromIntegral v))
  | otherwise -> StoryIndex (2 * fromIntegral v)

instance Show Addr where
  show (StoryIndex i) = printf "[%05x]" i

newtype Value = Value Int16 -- 16 bit signed values used for z-machine computations
  deriving (Ord,Eq,Integral,Real,Enum,Num,Bits,Ix)

instance Show Value where
  show (Value x) = show x
