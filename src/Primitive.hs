
-- | Pure primitives, typed & reified.
module Primitive (P1(..),P2(..),evalP1,evalP2) where

import Data.Bits ((.&.),(.|.),clearBit,setBit,testBit,shiftR)
import Numbers (Zversion,Byte,Value,Addr,makeByteAddress,makePackedAddress,makeHiLo,equalAny)
import qualified Data.Char as Char (chr,ord)
import qualified Lex (tokenize,lookupInStrings)

data P1 arg ret where

  Address :: P1 Value Addr
  DeAddress :: P1 Addr Value
  Div8 :: P1 Value Value
  EqualAny :: P1 [Value] Bool
  HiByte :: P1 Value Byte
  IsZero :: P1 Value Bool
  IsZeroAddress :: P1 Addr Bool
  IsZeroByte :: P1 Byte Bool
  LoByte :: P1 Value Byte
  PackedAddress :: Zversion -> P1 Value Addr
  SevenMinus :: P1 Byte Byte
  ShowNumber :: P1 Value String
  SingleChar :: P1 Value String
  StringBytes :: P1 String [Byte]
  StringLength :: P1 String Byte
  Tokenize :: P1 String (Byte,[(Byte,String)],String)
  Widen :: P1 Byte Value

deriving instance Show (P1 a b)

evalP1 :: P1 a r -> a -> r
evalP1 = \case
  Address -> makeByteAddress
  DeAddress -> fromIntegral
  Div8 -> \v -> v `div` 8
  EqualAny -> equalAny
  HiByte -> \v -> fromIntegral (v `shiftR` 8)
  IsZero -> (== 0)
  IsZeroAddress -> (== 0)
  IsZeroByte -> (== 0)
  LoByte -> \v -> fromIntegral (v .&. 0xff)
  PackedAddress zv -> makePackedAddress zv
  SevenMinus -> \v -> 7-v
  ShowNumber -> show
  SingleChar -> \v -> [Char.chr (fromIntegral v)]
  StringBytes -> \str -> [ fromIntegral (Char.ord c) | c <- str ]
  StringLength -> fromIntegral . length
  Tokenize -> Lex.tokenize
  Widen -> fromIntegral

data P2 arg1 arg2 ret where

  Add :: P2 Value Value Value
  And :: P2 Value Value Value
  BwAnd :: P2 Byte Byte Byte
  ClearBit :: P2 Byte Byte Byte
  Div :: P2 Value Value Value
  GreaterThan :: P2 Value Value Bool
  GreaterThanEqual :: P2 Value Value Bool
  LessThan :: P2 Value Value Bool
  LessThanByte :: P2 Byte Byte Bool
  LessThanEqual :: P2 Value Value Bool
  LookupInStrings :: P2 [String] String (Maybe Int)
  MakeHiLo :: P2 Byte Byte Value
  MinusByte :: P2 Byte Byte Byte
  Mod :: P2 Value Value Value
  Mul :: P2 Value Value Value
  Offset :: P2 Addr Value Addr
  Or :: P2 Value Value Value
  SetBit :: P2 Byte Byte Byte
  ShiftR :: P2 Byte Int Byte
  Sub :: P2 Value Value Value
  TestBit :: P2 Byte Byte Bool

deriving instance Show (P2 a b r)

evalP2 :: P2 a b r -> a -> b -> r
evalP2 = \case
  Add -> (+)
  And -> (.&.)
  BwAnd -> (.&.)
  ClearBit -> \b n -> b `clearBit` fromIntegral n
  Div -> div
  GreaterThan -> (>)
  GreaterThanEqual -> (>=)
  LessThan -> (<)
  LessThanByte -> (<)
  LessThanEqual -> (<=)
  LookupInStrings -> Lex.lookupInStrings
  MakeHiLo -> makeHiLo
  MinusByte -> (-)
  Mod -> mod
  Mul -> (*)
  Offset -> \base off -> base + fromIntegral off
  Or -> (.|.)
  SetBit -> \b n -> b `setBit` fromIntegral n
  ShiftR -> shiftR
  Sub -> (-)
  TestBit -> \b n -> b `testBit` fromIntegral n
