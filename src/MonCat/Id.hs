module MonCat.Id where

newtype Id x = Id { unId :: x }

instance Functor Id where
  fmap f (Id x) = Id (f x)
