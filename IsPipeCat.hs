{-# LANGUAGE PolyKinds #-}

module IsPipeCat (Cat(..), IsPipeCat(..)) where

import Prelude hiding (id, (.))
import Control.Category
import IsPipe


newtype Cat p m r i o = Cat { unCat :: p i o m r }

-- or with KindSignatures
-- newtype Cat (p :: * -> * -> (* -> *) -> * -> *) (m :: * -> *) r i o = Cat
--   { unCat :: p i o m r }

class IsPipe p => IsPipeCat p where
  (<+<) :: Monad m => p i' o m r -> p i i' m r -> p i o m r

instance (Monad m, IsPipeCat p) => Category (Cat p m r) where
  id = Cat idP
  (Cat p1) . (Cat p2) = Cat (p1 <+< p2)
