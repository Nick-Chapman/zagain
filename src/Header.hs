
-- | Header of a z-machine story file.
module Header(Header(..)) where

import Numbers (Addr,Zversion)

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
  deriving Show
