{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MonCat.FreeMonIn where

import Prelude hiding (id, curry, uncurry)

import GHC.Exts

import Control.Category
import Data.Function ((&))

import MonCat.Proxy
import MonCat.Nat
import MonCat.BiNat
import MonCat.Bifunctor
import MonCat.Day
import MonCat.Compose
import MonCat.PCom
import MonCat.MonCat
import MonCat.Id
import MonCat.Exp
import MonCat.MonInMonCat
import MonCat.FreeAp
import MonCat.Trivial

class FreeMonIn
  (free :: k -> k) (op :: k -> k -> k) (i :: k) (arr :: k -> k -> *) (obj :: k -> Constraint)
  | free -> op i arr obj, op -> free i arr obj where
  in1 :: forall a.
    i `arr` free a
  in2 :: forall a.
    (a `op` free a) `arr` free a
  cata :: forall a x.
    obj a =>
    (i `arr` x) -> ((a `op` x) `arr` x) -> free a `arr` x

  -- laws
  -- cata a b . in1 = a
  -- cata a b . in2 = b . (id ⊗ cata a b)


instance FreeMonIn [] (,) () (->) Trivial where
  in1 :: () -> [a]
  in1 _ = []
  in2 :: (a, [a]) -> [a]
  in2 (a, l) = a : l
  cata :: (() -> x) -> ((a , x) -> x) -> ([a] -> x)
  cata a _ []     = a ()
  cata a b (x:xs) = b ((id ⊗ cata a b) (x, xs))

instance FreeMonIn FreeAp (⋆) Id (⇉) Functor where
  in1 :: Id ⇉ FreeAp f
  in1 = Nat (\(Id x) -> Pure x)
  in2 :: f ⋆ FreeAp f ⇉ FreeAp f
  in2 = Nat Rec
  cata :: (Functor f) => (Id ⇉ x) -> (f ⋆ x ⇉ x) -> (FreeAp f ⇉ x)
  cata a b = Nat f
    where
    f (Pure x)  = unNat a (Id x)
    f (Rec day) = unNat b (unNat (id ⊗ cata a b) day)

hoist :: forall a b free op i arr obj.
  obj a =>
  Category arr =>
  Bifunctor op arr obj =>
  FreeMonIn free op i arr obj =>
  (a `arr` b) -> free a `arr` free b
hoist x = cata f1 f2
  where
  f1 :: i `arr` free b
  f1 = in1
  f2 :: (a `op` free b) `arr` free b
  f2 =
    -- a ⊗ free b
    (x ⊗ id) >>>
    -- b ⊗ free b
    in2
    -- free b

ins :: forall a arr op i free obj.
  obj a =>
  Category arr =>
  Bifunctor op arr obj =>
  FreeMonIn free op i arr obj =>
  MonCat op i arr obj =>
  a `arr` free a
ins =
  -- a
  ρ1 >>>
  -- a ⊗ i
  (id ⊗ in1) >>>
  -- a ⊗ free a
  in2
  -- free a

mu :: forall op i arr obj exp a free.
  obj a =>
  obj (free a `exp` free a) =>
  obj (free a) =>
  Category arr =>
  Bifunctor op arr obj =>
  FreeMonIn free op i arr obj =>
  Exp exp op arr obj =>
  MonCat op i arr obj =>
  (free a `op` free a) `arr` free a
mu = uncurry (cata (curry f1) (curry f2))
  where
  f1 :: forall x. obj x => (i `op` x) `arr` x
  f1 = λ
  f2 :: ((a `op` (free a `exp` free a)) `op` free a) `arr` free a
  f2 =
    -- (a ⊗ (free a `exp` free a)) ⊗ free a
    α1 >>>
    -- a ⊗ ((free a `exp` free a) ⊗ free a)
    (id ⊗ ev) >>>
    -- a ⊗ free a
    in2
    -- free a

free :: forall op i arr obj a m free monoid.
  obj a =>
  monoid m =>
  Category arr =>
  Bifunctor op arr obj =>
  FreeMonIn free op i arr obj =>
  MonInMonCat m op i arr monoid =>
  (a `arr` m) -> free a `arr` m
free f =
  cata f1 f2
  where
  f1 = η (Proxy :: Proxy monoid)
  f2 =
    -- a ⊗ m
    (f ⊗ id) >>>
    -- m ⊗ m
    μ
    -- m
