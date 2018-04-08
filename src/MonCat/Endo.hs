module MonCat.Endo where

newtype Endo a = Endo { unEndo :: a -> a}

instance Monoid (Endo a) where
  mempty = Endo id
  mappend (Endo a) (Endo b) = Endo (a . b)
