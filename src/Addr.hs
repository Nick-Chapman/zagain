
module Addr (Addr,ofPackedWord) where

import Text.Printf (printf)

newtype Addr = Addr Int
  deriving (Ord,Eq,Num,Integral,Real,Enum)

ofPackedWord :: Word -> Addr
ofPackedWord w = Addr (2 * fromIntegral w)

instance Show Addr where
  --show (Addr i) = printf "[%05i]" i -- TODO: instead of caller doing this
  show (Addr i) = printf "%05i" i
