{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}

module MonCat.PCom where

data PCom (p :: * -> * -> *) (q :: * -> * -> *) (x :: *) (y :: *)
  where
  PCom ::
    p x z ->
    q z y ->
    (p `PCom` q) x y
