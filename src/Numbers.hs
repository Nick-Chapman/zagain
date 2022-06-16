
-- | Various types of numbers used by the z-machine.
module Numbers
  ( Zversion(..)
  , Byte
  , Addr, makeByteAddress, makeWordAddress, makePackedAddress
  , Value, makeHiLo
  , Style(..)
  ) where

import Data.Bits (Bits)
import Data.Int (Int16)
import Data.Word (Word8)
import GHC.Ix (Ix)
import Text.Printf (printf)

data Zversion = Z1 | Z2 | Z3 | Z4 | Z5 | Z6
  deriving (Eq,Ord,Show)

newtype Byte = EightBits Word8
  deriving (Ord,Eq,Integral,Real,Enum,Num,Bits)

instance Show Byte where
  --show (EightBits w8) = printf "0x%02x" w8
  show (EightBits w8) = printf "%i" w8

newtype Addr = StoryIndex Word
  deriving (Ord,Eq,Num,Integral,Real,Enum,Bits,Ix)

makeByteAddress :: Value -> Addr
makeByteAddress v = StoryIndex (asUnsigned v)

makeWordAddress :: Value -> Addr
makeWordAddress v = StoryIndex (2 * asUnsigned v)

makePackedAddress :: Zversion -> Value -> Addr
makePackedAddress zv v = StoryIndex (m * asUnsigned v)
  where m = if zv <= Z3 then 2 else 4

asUnsigned :: Value -> Word
asUnsigned v = if
  | v < 0 -> 0x10000 + fromIntegral v
  | otherwise -> fromIntegral v

instance Show Addr where
  --show (StoryIndex i) = printf "[%05x]" i
  show (StoryIndex i) = printf "%06i" i

newtype Value = Value Int16 -- 16 bit signed values used for z-machine computations
  deriving (Ord,Eq,Integral,Real,Enum,Num,Bits,Ix)

instance Show Value where
  show (Value x) = show x

makeHiLo :: Byte -> Byte -> Value
makeHiLo hi lo = 256 * fromIntegral hi + fromIntegral lo

data Style = Reverse | Bold | Italic | Fixed
  deriving (Eq,Ord,Show)
