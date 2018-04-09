{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}

module MonCat.FreeMon where

import Prelude hiding (id)

import GHC.Exts

import Control.Category
import Data.Function ((&))

import MonCat.Bifunctor
import MonCat.MonCat
import MonCat.MonInMonCat

class FreeMon
  (free :: k -> k) (op :: k -> k -> k) (i :: k) (arr :: k -> k -> *) (monoid :: k -> Constraint)
  | free -> op i arr monoid where
  ins :: forall a.
    a `arr` free a
  free :: forall a m.
    (monoid m) =>
    -- the second `arr` is a monoid homomorphism, see laws below
    (a `arr` m) -> free a `arr` m

  -- laws
  -- free f . ins = f
  -- monoid homomorphism laws
  --   ( interpreting to monoid in monoidal category m
  --   , where its η = e and μ = m )
  -- free f . η = e
  -- free f . μ = m . (free f ⊗ free f)

instance FreeMon [] (,) () (->) Monoid where
  ins :: a -> [a]
  ins a = [a]
  free :: Monoid m => (a -> m) -> ([a] -> m)
  free _ []     = mempty
  free f (x:xs) = f x `mappend` free f xs

in2 ::
  obj a =>
  monoid (free a) =>
  Category arr =>
  Bifunctor op arr obj =>
  FreeMon free op i arr monoid =>
  MonInMonCat (free a) op i arr monoid =>
  (a `op` free a) `arr` free a
in2 =
  -- a `op` free a
  (ins `bimap` id) >>>
  -- free a `op` free a
  μ
  -- free a
