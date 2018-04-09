{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MonCat.Exp where

import Prelude hiding (id, curry, uncurry)

import GHC.Exts

import Control.Category

import MonCat.Trivial
import MonCat.Nat
import MonCat.BiNat
import MonCat.Day
import MonCat.Compose
import MonCat.PCom

import MonCat.MonCat
import MonCat.Bifunctor
import MonCat.MonInMonCat

class Exp
  (exp :: k -> k -> k) (op :: k -> k -> k) (arr :: k -> k -> *) (obj :: k -> Constraint)
  | op arr -> exp obj where
  curry :: forall (x :: k) (a :: k) (b :: k).
    ((x `op` a) `arr` b) -> (x `arr` (a `exp` b))

  uncurry :: forall (x :: k) (a :: k) (b :: k).
    (obj b) =>
    (x `arr` (a `exp` b)) -> ((x `op` a) `arr` b)

ev :: forall exp op arr obj a b.
  Category arr =>
  Exp exp op arr obj =>
  (obj a) =>
  ((b `exp` a) `op` b) `arr` a
ev = uncurry id

{-
rep :: forall (m :: k) (exp :: k -> k -> k)
    (op :: k -> k -> k) (i :: k) (arr :: k -> k -> *)
    (obj :: k -> Constraint) (monoid :: k -> Constraint).
  Category arr =>
  MonCat op i arr obj =>
  MonInMonCat m op i arr monoid =>
  Exp exp op arr obj =>
  monoid m =>
  m `arr` (m `exp` m)
rep =
  (curry :: (m `op` m) `arr` m -> m `arr` (m `exp` m))
  (μ :: (m `op` m) `arr` m)
-}

instance Exp (->) (,) (->) Trivial where
  curry ::
    ((x , a) -> b) ->
    (x -> a -> b)
  curry f x a = f (x, a)
  uncurry ::
    (x -> (a -> b)) ->
    ((x , a) -> b)
  uncurry f (x, a) = f x a

data ExpDay f g b = ExpDay { unExp :: forall a. f a -> g (b, a) }

instance Exp ExpDay Day Nat Functor where
  curry ::
    ((x `Day` a) `Nat` b) ->
    (x `Nat` (a `ExpDay` b))
  curry (Nat m) = Nat (\x -> ExpDay (\y -> m (Day x y id)))
  uncurry :: (Functor b) =>
    (x `Nat` (a `ExpDay` b)) ->
    ((x `Day` a) `Nat` b)
  uncurry (Nat f) = Nat (\(Day x y h) -> fmap h (unExp (f x) y))
