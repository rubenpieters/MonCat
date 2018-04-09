{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}

module MonCat.Compose where

data Compose (f :: * -> *) (g :: * -> *) (x :: *)
  = Compose (f (g x))

instance (Functor f, Functor g) => Functor (f `Compose` g) where
  fmap :: forall a b.
    (a -> b) -> (f `Compose` g) a -> (f `Compose` g) b
  fmap f (Compose fga) =
    Compose ((\ga -> f <$> ga) <$> fga)
