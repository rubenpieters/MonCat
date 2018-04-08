{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MonCat.MonCatSpec (main, spec) where

import Prelude hiding (id)

import MonCat.MonCat
import MonCat.Bifunctor

import Control.Category

import Test.Hspec
import Test.QuickCheck

-- laws monoidal categories

-- triangle diagram

triangle1 ::
  Category arr =>
  Bifunctor op arr obj =>
  MonCat op i arr obj =>
  obj a =>
  obj (a `op` i) =>
  (a `op` (i `op` b)) `arr` (a `op` b)
triangle1 = α >>> (ρ ⊗ id)

triangle2 ::
  Category arr =>
  Bifunctor op arr obj =>
  MonCat op i arr obj =>
  obj a =>
  obj b =>
  (a `op` (i `op` b)) `arr` (a `op` b)
triangle2 = id ⊗ λ

-- pentagon diagram

-- ...

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "monoidal category C" $ do
    it "satisfies triangle diagram" $ property $ do
      \(a :: Int) (b :: Int) ->
        (triangle1 :: (a, ((), b)) -> (a, b)) (a, ((), b))
        `shouldBe`
        (triangle2 :: (a, ((), b)) -> (a, b)) (a, ((), b))
