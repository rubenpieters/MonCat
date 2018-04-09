{-# LANGUAGE TypeOperators #-}

module MonCat.FreeAr where

import MonCat.PCom
import MonCat.Profunctor
import MonCat.WeakArrow

data FreeAr a x y = Hom (x -> y) | Comp ((a `PCom` FreeAr a) x y)

instance Profunctor a => Profunctor (FreeAr a) where
  dimap f g (Hom h) = Hom (g . h .f)
  dimap f g (Comp (PCom x y)) = Comp (PCom (dimap f id x) (dimap id g y))

instance Profunctor a => WeakArrow (FreeAr a) where
  arr = Hom
  (Hom f) >>> c = dimap f id c
  (Comp (PCom x y)) >>> c = Comp $ PCom x (y >>> c)
