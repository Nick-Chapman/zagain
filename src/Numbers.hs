
-- | Various types of numbers used by the z-machine.
module Numbers
  ( Zversion(..)
  , Byte
  , Addr, makePackedAddress, makeByteAddress
  , Value, makeHiLo, equalAny
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
  show (EightBits w8) = printf "0x%02x" w8
  --show (EightBits w8) = printf "%i" w8

newtype Addr = StoryIndex Word
  deriving (Ord,Eq,Num,Integral,Real,Enum,Bits,Ix)

packAdressMultiplier :: Zversion -> Int
packAdressMultiplier v = if v <= Z3 then 2 else 4

makePackedAddress :: Zversion -> Value -> Addr
makePackedAddress zv v = if
  | v < 0 -> StoryIndex (fromIntegral m * (0x10000 + fromIntegral v))
  | otherwise -> StoryIndex (fromIntegral (m * fromIntegral v))
    where
      m = packAdressMultiplier zv

makeByteAddress :: Value -> Addr
makeByteAddress v = if
  | v < 0 -> StoryIndex (0x10000 + fromIntegral v)
  | otherwise -> StoryIndex (fromIntegral v)

instance Show Addr where
  show (StoryIndex i) = printf "[%05x]" i

newtype Value = Value Int16 -- 16 bit signed values used for z-machine computations
  deriving (Ord,Eq,Integral,Real,Enum,Num,Bits,Ix)

instance Show Value where
  show (Value x) = show x

makeHiLo :: Byte -> Byte -> Value
makeHiLo hi lo = 256 * fromIntegral hi + fromIntegral lo

equalAny :: [Value] -> Bool
equalAny = \case
  [v1,v2] -> v1==v2
  [v1,v2,v3] -> v1==v2 || v1==v3
  [v1,v2,v3,v4] -> v1==v2 || v1==v3 || v1==v4
  vs -> error (show ("EqualAny",vs))
