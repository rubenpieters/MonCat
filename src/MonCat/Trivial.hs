{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module MonCat.Trivial where

class Trivial a where
instance Trivial a where
