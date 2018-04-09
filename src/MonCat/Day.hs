{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}

module MonCat.Day where

data Day (f :: * -> *) (g :: * -> *) (x :: *)
  where
  Day :: forall f g x y z.
    f y ->
    g z ->
    ((y, z) -> x) ->
    (f `Day` g) x

instance Functor (f `Day` g) where
  fmap :: forall a b.
    (a -> b) -> (f `Day` g) a -> (f `Day` g) b
  fmap f (Day fy gz yzx) =
    Day fy gz (f . yzx)

