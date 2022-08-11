
-- | Simple Hetrogenous Maps
module HMap (empty,newKey,lookup,extend) where

import Data.Dynamic (Typeable,Dynamic,toDyn,fromDynamic)
import Data.Map (Map)
import Data.Unique (Unique,newUnique)
import Prelude hiding (lookup)
import qualified Data.Map as Map

data Env = Env (Map Unique Dynamic)
data Key a = Key Unique

newKey :: IO (Key a)
newKey = Key <$> newUnique

empty :: Env
empty = Env Map.empty

lookup :: Typeable a => Env -> Key a -> Maybe a
lookup (Env m) (Key u) = Map.lookup u m >>= fromDynamic

extend :: Typeable a => Env -> Key a -> a -> Env
extend (Env m) (Key u) x = Env (Map.insert u (toDyn x) m)
