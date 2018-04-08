{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module MonCat.MonInMonCat where

import GHC.Exts
import Control.Applicative
import Control.Monad

import MonCat.Proxy
import MonCat.Nat
import MonCat.BiNat
import MonCat.Id
import MonCat.Day
import MonCat.Compose
import MonCat.PCom
import MonCat.WeakArrow

class MonInMonCat
  (a :: k) (op :: k -> k -> k) (i :: k) (arr :: k -> k -> *) (monoid :: k -> Constraint)
  | op -> i arr monoid, monoid -> op i arr where
  η ::
    (monoid a) =>
    Proxy monoid -> i `arr` a
  μ ::
    (monoid a) =>
    (a `op` a) `arr` a

instance MonInMonCat a (,) () (->) Monoid where
  η _ = (\() -> mempty)
    :: Monoid m => () -> m
  μ = (\(a, b) -> a `mappend` b)
    :: Monoid a => (a , a) -> a

instance MonInMonCat f (⋆) Id (⇉) Applicative where
  η _ = Nat (\(Id a) -> pure a)
    :: Applicative f => Id ⇉ f
  μ = Nat (\(Day a b f) -> liftA2 (curry f) a b)
    :: Applicative f => (f ⋆ f) ⇉ f

instance MonInMonCat m (○) Id (⇉) Monad where
  η _ = Nat (\(Id a) -> return a)
    :: Monad m => Id ⇉ m
  μ = Nat (\(Compose mm) -> join mm)
    :: Monad m => (m ○ m) ⇉ m

instance MonInMonCat p (⊛) (->) (⇶) WeakArrow where
  η _ = BiNat arr
    :: WeakArrow p => (->) ⇶ p
  μ = BiNat (\(PCom p q) -> p >>> q)
    :: WeakArrow p => (p ⊛ p) ⇶ p
