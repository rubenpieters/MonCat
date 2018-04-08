{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

module MonCat.Nat where

import Prelude hiding (id, (.))

import Control.Category

newtype (⇉) f g = Nat
  { unNat :: forall a.
    f a -> g a
  }

instance Category (⇉) where
  id = Nat id
  Nat f . Nat g = Nat (f . g)
