
module Numbers
  ( Byte, byteOfValue
  , Word
  , Addr, addrOfPackedWord
  , Value
  ) where

import Data.Bits (Bits)
import Data.Int (Int16)
import Data.Word (Word8,Word16)
import GHC.Ix (Ix)
import Prelude hiding (Word)
import Text.Printf (printf)
import qualified Prelude (Word)

matchNizFormat :: Bool
matchNizFormat = True -- False for better debug/dev experience

newtype Byte = EightBits Word8
  deriving (Ord,Eq,Integral,Real,Enum,Num,Bits)

instance Show Byte where
  show (EightBits w8) = if
    | matchNizFormat -> show w8
    | otherwise -> printf "0x%02x" w8

byteOfValue :: Value -> Byte
byteOfValue v = do
  if v < 0 || v > 255 then error (show ("byteOfValue",v)) else fromIntegral v

{-
newtype Word = SixteenBits Word16
  deriving (Ord,Eq,Num,Integral,Real,Enum,Ix,Bits)

instance Show Word where
  show (SixteenBits w16) = if
    | matchNizFormat -> show w16
    | otherwise -> printf "0x%04x" w16
-}
type Word = Value

newtype Addr = StoryIndex Prelude.Word
  deriving (Ord,Eq,Num,Integral,Real,Enum)

addrOfPackedWord :: Word -> Addr
addrOfPackedWord w = StoryIndex (2 * fromIntegral w)

instance Show Addr where
  show (StoryIndex i) = if
    | matchNizFormat -> printf "%05i" i
    | otherwise -> printf "[%05i]" i

newtype Value = Value Int16 -- for signed numeric operations
  deriving (Ord,Eq,Integral,Real,Enum,Num,Bits,Ix)

instance Show Value where
  show (Value x) = if
    | matchNizFormat -> show (fromIntegral x :: Word16) -- show unsigned
    | otherwise -> show x
