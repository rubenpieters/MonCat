{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module MonCat.Bifunctor where

import GHC.Exts

import MonCat.Trivial
import MonCat.Nat
import MonCat.BiNat
import MonCat.Day
import MonCat.Compose
import MonCat.PCom
import MonCat.Profunctor

class Bifunctor
  (op :: k -> k -> k) (arr :: k -> k -> *) (obj :: k -> Constraint)
  | op -> arr obj where

  bimap :: forall (a :: k) (b :: k) (c :: k) (d :: k).
    (obj a) =>
    (a `arr` c) ->
    (b `arr` d) ->
    ((a `op` b) `arr` (c `op` d))

instance Bifunctor (,) (->) Trivial where
  bimap :: forall a b c d.
    (a -> c) ->
    (b -> d) ->
    ((a , b) -> (c , d))
  bimap f g (a, b) = (f a, g b)

instance Bifunctor Day Nat Functor where
  bimap :: forall a b c d.
    (a `Nat` c) ->
    (b `Nat` d) ->
    ((a `Day` b) `Nat` (c `Day` d))
  bimap (Nat f) (Nat g) =
    Nat (\(Day a b h) -> Day (f a) (g b) h)

instance Bifunctor Compose Nat Functor where
  bimap :: forall a b c d.
    (Functor a) =>
    (a `Nat` c) ->
    (b `Nat` d) ->
    ((a `Compose` b) `Nat` (c `Compose` d))
  bimap (Nat f) (Nat g) =
    Nat (\(Compose abx) -> Compose (f (g <$> abx)))

instance Bifunctor PCom BiNat Profunctor where
  bimap :: forall a b c d.
    (a `BiNat` c) ->
    (b `BiNat` d) ->
    ((a `PCom` b) `BiNat` (c `PCom` d))
  bimap (BiNat f) (BiNat g) =
    BiNat (\(PCom axz bzy) -> PCom (f axz) (g bzy))

