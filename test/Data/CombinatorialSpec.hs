module Data.CombinatorialSpec where

import Control.Monad
import Data.Combinatorial
import Data.List
import Test.Hspec

spec :: Spec
spec = describe "Data.Combinatorial" $ do
  describe "carthesian" $ do
    it "combines every (x, y) pair combination" $ do
      carthesian [] [] `shouldBe` ([] :: [(Int, Int)])
      carthesian [1] [1] `shouldBe` [(1, 1)]
      carthesian [1, 2] [1, 2] `shouldBe` [(1, 1), (1, 2), (2, 1), (2, 2)]
      carthesian [1, 2] [1, 2, 3] `shouldBe` [(1, 1), (1, 2), (1, 3), (2, 1), (2, 2), (2, 3)]
    it "length should be the product of the length of both inputs" $ do
      length (carthesian [1 .. 100] [1 .. 10]) `shouldBe` (length [1 .. 100] * length [1 .. 10])
      length (carthesian [1 .. 1000] [1 .. 2]) `shouldBe` (length [1 .. 2] * length [1 .. 1000])
  describe "combinatorial" $ do
    it "length is length of c powered to (length a * length b)" $ do
      genericLength (combinatorial [1 .. 100] [1 .. 100] [])
        `shouldBe` genericLength []
        ^          (genericLength [1 .. 100] * genericLength [1 .. 100])
      genericLength (combinatorial [1 .. 10] [1 .. 10] [1])
        `shouldBe` genericLength [1]
        ^          (genericLength [1 .. 100] * genericLength [1 .. 100])
      genericLength (combinatorial [1 .. 100] [1 .. 100] [2])
        `shouldBe` genericLength [2]
        ^          (genericLength [1 .. 100] * genericLength [1 .. 100])
    it "returns all distinct (x, y, z) combinations" $ do
      combinatorial [1] [1] [1] `shouldBe` [[(1, 1, 1)]]
      combinatorial [1 .. 2] [1 .. 2] [1] `shouldBe` [[(1, 1, 1), (1, 2, 1), (2, 1, 1), (2, 2, 1)]]
      forM_ [[(1, 1, 1), (1, 2, 1)], [(1, 1, 2), (1, 2, 2)], [(1, 1, 1), (1, 2, 2)], [(1, 1, 2), (1, 2, 1)]] $ \x -> do
        combinatorial [1] [1 .. 2] [1, 2] `shouldSatisfy` elem x
