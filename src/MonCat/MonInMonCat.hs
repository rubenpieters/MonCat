{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
    i `arr` a
  μ ::
    (monoid a) =>
    (a `op` a) `arr` a

instance MonInMonCat a (,) () (->) Monoid where
  η = (\() -> mempty)
    :: Monoid m => () -> m
  μ = (\(a, b) -> a `mappend` b)
    :: Monoid a => (a , a) -> a

instance MonInMonCat f Day Id Nat Applicative where
  η = Nat (\(Id a) -> pure a)
    :: Applicative f => Id `Nat` f
  μ = Nat (\(Day a b f) -> liftA2 (curry f) a b)
    :: Applicative f => (f `Day` f) `Nat` f

instance MonInMonCat m Compose Id Nat Monad where
  η = Nat (\(Id a) -> return a)
    :: Monad m => Id `Nat` m
  μ = Nat (\(Compose mm) -> join mm)
    :: Monad m => (m `Compose` m) `Nat` m

instance MonInMonCat p PCom (->) BiNat WeakArrow where
  η = BiNat arr
    :: WeakArrow p => (->) `BiNat` p
  μ = BiNat (\(PCom p q) -> p >>> q)
    :: WeakArrow p => (p `PCom` p) `BiNat` p
