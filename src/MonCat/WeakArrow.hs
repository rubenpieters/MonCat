module MonCat.WeakArrow where

import MonCat.Profunctor

class Profunctor a => WeakArrow a where
  arr :: (x -> y) -> a x y
  (>>>) :: a x y -> a y z -> a x z
