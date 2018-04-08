module MonCat.Profunctor where

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> (p b c -> p a d)
