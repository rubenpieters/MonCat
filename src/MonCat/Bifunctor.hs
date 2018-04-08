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

  (⊗) :: forall (a :: k) (b :: k) (c :: k) (d :: k).
    (obj a) =>
    (a `arr` c) ->
    (b `arr` d) ->
    ((a `op` b) `arr` (c `op` d))

instance Bifunctor (,) (->) Trivial where
  (⊗) :: forall a b c d.
    (a -> c) -> (b -> d) -> ((a , b) -> (c , d))
  (⊗) f g (a, b) = (f a, g b)

instance Bifunctor (⋆) (⇉) Functor where
  (⊗) :: forall a b c d.
    (a ⇉ c) -> (b ⇉ d) -> ((a ⋆ b) ⇉ (c ⋆ d))
  (⊗) (Nat f) (Nat g) =
    Nat (\(Day a b h) -> Day (f a) (g b) h)

instance Bifunctor (○) (⇉) Functor where
  (⊗) :: forall a b c d.
    (Functor a) =>
    (a ⇉ c) -> (b ⇉ d) -> ((a ○ b) ⇉ (c ○ d))
  (⊗) (Nat f) (Nat g) =
    Nat (\(Compose abx) -> Compose (f (g <$> abx)))

instance Bifunctor (⊛) (⇶) Profunctor where
  (⊗) :: forall a b c d.
    (a ⇶ c) -> (b ⇶ d) -> ((a ⊛ b) ⇶ (c ⊛ d))
  (⊗) (BiNat f) (BiNat g) =
    BiNat (\(PCom axz bzy) -> PCom (f axz) (g bzy))

