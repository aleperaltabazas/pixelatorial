{-# LANGUAGE OverloadedStrings #-}

module Data.BigIntSpec where

import Control.Monad
import Data.BigInt
import Test.Hspec

spec :: Spec
spec = describe "Data.BigInt" $ do
  describe "Show" $ do
    it "returns a string with equal length as its wrapped list" $ do
      (length . show) (BigInt [1, 0, 0, 0]) `shouldBe` 4
      (length . show) (BigInt []) `shouldBe` 0
    it "joins all the numbers from right to left" $ do
      show (BigInt []) `shouldBe` ""
      show (BigInt [1, 2, 3, 4]) `shouldBe` "4321"
  describe "incr" $ do
    it "adds one to the leftmost value" $ do
      incr (BigInt [0, 1]) `shouldBe` BigInt [1, 1]
      incr (BigInt [0]) `shouldBe` BigInt [1]
    it "overflows whenever any element reaches 10" $ do
      incr (BigInt [9]) `shouldBe` BigInt [0, 1]
      incr (BigInt [9, 1]) `shouldBe` BigInt [0, 2]
