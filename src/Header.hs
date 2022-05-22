
-- | Header of a z-machine story file.
module Header(Header(..),Zversion(..)) where

import Numbers (Addr)

data Header = Header
  { zv :: Zversion
  , highMem :: Addr
  , initialPC :: Addr
  , dictionary :: Addr
  , objectTable :: Addr
  , globalVars :: Addr
  , staticMem :: Addr
  , abbrevTable :: Addr
  }

data Zversion = Z1 | Z2 | Z3 | Z4 | Z5 | Z6
  deriving (Eq,Show)
