{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}

module MonCat.Day where

data (⋆) (f :: * -> *) (g :: * -> *) (x :: *)
  where
  Day :: forall f g x y z.
    f y ->
    g z ->
    ((y, z) -> x) ->
    (f ⋆ g) x

instance Functor (f ⋆ g) where
  fmap :: forall a b.
    (a -> b) -> (f ⋆ g) a -> (f ⋆ g) b
  fmap f (Day fy gz yzx) =
    Day fy gz (f . yzx)

