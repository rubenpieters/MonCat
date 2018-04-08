{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

module MonCat.FreeAp where

import MonCat.Day

data FreeAp f x = Pure x | Rec ((f ⋆ FreeAp f) x)
  deriving (Functor)

instance Applicative (FreeAp f) where
  pure = Pure
  (Pure g) <*> z = fmap g z
  (Rec (Day fy gz yzx)) <*> z = Rec (Day fy (pure (,) <*> gz <*> z) (\(a, (b, c)) -> yzx (a, b) c))
