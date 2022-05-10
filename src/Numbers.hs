
module Numbers
  ( Byte
  , Addr, addrOfPackedWord
  , Value, valueToByte, valueOfByte, valueOfWord, valueToAddr, valueToWord, valueOfInt
  ) where

import Text.Printf (printf)
import Data.Int (Int16)
import Data.Word (Word8)

--[byte]--------------------------------------------------------------

type Byte = Word8

--[address: index into story file]------------------------------------

newtype Addr = Addr Int
  deriving (Ord,Eq,Num,Integral,Real,Enum)

addrOfPackedWord :: Word -> Addr
addrOfPackedWord w = Addr (2 * fromIntegral w)

instance Show Addr where
  --show (Addr i) = printf "[%05i]" i -- TODO: instead of caller doing this
  show (Addr i) = printf "%05i" i

--[z-value]-----------------------------------------------------------

type Value = Int16

valueToWord :: Value -> Word
valueToWord v = do
  -- fromIntegral just does the _right thing_ :)
  let w :: Word = fromIntegral v -- .&. 0xffff -- .&. needed for show
  --if v < 0 then error (show ("valueToWord(is-neg)",v,w)) else
  w

valueToAddr :: Value -> Addr
valueToAddr v = fromIntegral (valueToWord v) -- always ok

valueToByte :: Value -> Byte
valueToByte v = do
  if v < 0 || v > 255 then error (show ("valueToByte",v)) else fromIntegral v

valueOfWord :: Word -> Value
valueOfWord w = do
  let v :: Value = fromIntegral w
  --if w `testBit` 15 then error (show ("valueOfWord",w,v)) else
  v

valueOfByte :: Byte -> Value
valueOfByte b = fromIntegral b -- always ok

valueOfInt :: Int -> Value --TODO: why do we ever have ints?
valueOfInt = fromIntegral
