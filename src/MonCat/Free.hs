{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

module MonCat.Free where

import MonCat.Compose

data Free f x = Ret x | Con ((f `Compose` Free f) x)
  deriving (Functor)

instance (Functor f) => Applicative (Free f) where
  pure = Ret
  (Ret x) <*> f = fmap x f
  (Con (Compose m)) <*> f = Con (Compose (fmap (<*> f) m))

instance (Functor f) => Monad (Free f) where
  return = Ret
  (Ret x) >>= f = f x
  (Con (Compose m)) >>= f = Con (Compose (fmap (>>= f) m))
