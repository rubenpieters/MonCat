{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module MonCat.MonCat where

import GHC.Exts

import MonCat.Trivial
import MonCat.Nat
import MonCat.BiNat
import MonCat.Id
import MonCat.Day
import MonCat.Compose
import MonCat.PCom
import MonCat.Bifunctor
import MonCat.Profunctor

class MonCat
  (op :: k -> k -> k) (i :: k) (arr :: k -> k -> *) (obj :: k -> Constraint)
  | op -> i arr obj where
  α :: forall a b c.
    (obj a) =>
    (a `op` (b `op` c)) `arr` ((a `op` b) `op` c)
  α1 :: forall a b c.
    (obj a, obj b, obj c) =>
    ((a `op` b) `op` c) `arr` (a `op` (b `op` c))
  λ :: forall a.
    (obj a) =>
    (i `op` a) `arr` a
  λ1 :: forall a.
    (obj a) =>
    a `arr` (i `op` a)
  ρ :: forall a.
    (obj a) =>
    (a `op` i) `arr` a
  ρ1 :: forall a.
    (obj a) =>
    a `arr` (a `op` i)

instance MonCat (,) () (->) Trivial where
  α = (\(a, (b, c)) -> ((a, b), c))
    :: (a , (b , c)) -> ((a , b) , c)
  α1 = (\((a, b), c) -> (a, (b, c)))
    :: ((a , b) , c) -> (a , (b , c))
  λ = (\((), a) -> a)
    :: (() , a) -> a
  λ1 = (\a -> ((), a))
    :: a -> (() , a)
  ρ = (\(a, ()) -> a)
    :: (a , ()) -> a
  ρ1 = (\a -> (a, ()))
    :: a -> (a , ())

instance MonCat (⋆) Id (⇉) Functor where
  α = Nat (\(Day a (Day b c f) g) -> Day (Day a b (\(x,y) -> (curry f y, x))) c (\((v, w), z) -> curry g w (v z)))
    :: (a ⋆ (b ⋆ c)) ⇉ ((a ⋆ b) ⋆ c)
  α1 = Nat (\(Day (Day a b f) c g) -> Day a (Day b c (\(x, y) -> (flip (curry f) x, y))) (\(z, (v, w)) -> curry g (v z) w))
    :: ((a ⋆ b) ⋆ c) ⇉ (a ⋆ (b ⋆ c))
  λ = Nat (\(Day (Id x) a f) -> curry f x <$> a)
    :: (Functor a) => (Id ⋆ a) ⇉ a
  λ1 = Nat (\a -> Day (Id ()) a snd)
    :: a ⇉ (Id ⋆ a)
  ρ = Nat (\(Day a (Id x) f) -> flip (curry f) x <$> a)
    :: (Functor a) => (a ⋆ Id) ⇉ a
  ρ1 = Nat (\a -> Day a (Id ()) fst)
    :: a ⇉ (a ⋆ Id)

instance MonCat (○) Id (⇉) Functor where
  α = Nat (\(Compose mm) -> Compose $ Compose $ (\(Compose m) -> m) <$> mm)
    :: (Functor a) => (a ○ (b ○ c)) ⇉ ((a ○ b) ○ c)
  α1 = Nat (\(Compose mm) -> Compose $ Compose <$> (\(Compose m) -> m) mm)
    :: (Functor a) => ((a ○ b) ○ c) ⇉ (a ○ (b ○ c))
  λ = Nat (\(Compose ia) -> unId ia)
    :: (Id ○ a) ⇉ a
  λ1 = Nat (\a -> Compose (Id a))
    :: a ⇉ (Id ○ a)
  ρ = Nat (\(Compose ai) -> unId <$> ai)
    :: (Functor a) => (a ○ Id) ⇉ a
  ρ1 = Nat (\a -> Compose (Id <$> a))
    :: (Functor a) => a ⇉ (a ○ Id)

instance MonCat (⊛) (->) (⇶) Profunctor where
  α = BiNat (\(PCom p (PCom q r)) -> PCom (PCom p q) r)
    :: (a ⊛ (b ⊛ c)) ⇶ ((a ⊛ b) ⊛ c)
  α1 = BiNat (\(PCom (PCom p q) r) -> PCom p (PCom q r))
    :: ((a ⊛ b) ⊛ c) ⇶ (a ⊛ (b ⊛ c))
  λ = BiNat (\(PCom f a) -> dimap f id a)
    :: (Profunctor a) => ((->) ⊛ a) ⇶ a
  λ1 = BiNat (\a -> PCom id a)
    :: a ⇶ ((->) ⊛ a)
  ρ = BiNat (\(PCom a f) -> dimap id f a)
    :: (Profunctor a) => (a ⊛ (->)) ⇶ a
  ρ1 = BiNat (\a -> PCom a id)
    :: a ⇶ (a ⊛ (->))
