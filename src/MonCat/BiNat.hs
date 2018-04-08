{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

module MonCat.BiNat where

import Prelude hiding (id, (.))

import Control.Category

newtype (⇶) f g = BiNat
  { unBiNat :: forall a b.
    f a b -> g a b
  }

instance Category (⇶) where
  id = BiNat id
  BiNat f . BiNat g = BiNat (f . g)
