{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}

module MonCat.Compose where

data (○) (f :: * -> *) (g :: * -> *) (x :: *)
  = Compose (f (g x))

instance (Functor f, Functor g) => Functor (f ○ g) where
  fmap :: forall a b.
    (a -> b) -> (f ○ g) a -> (f ○ g) b
  fmap f (Compose fga) =
    Compose ((\ga -> f <$> ga) <$> fga)
