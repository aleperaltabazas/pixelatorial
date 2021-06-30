module Codec.Picture.SVGSpec
  ( spec
  )
where

import Codec.Picture.SVG
import Test.Hspec

spec :: Spec
spec = describe "Codec.Picture.SVGSpec" $ do
  describe "encodePixel"
    $ it "returns a <rect/> tag in the given coordinates with the given color and a width and height of 1"
    $ do
        encodePixel (0, 0, "#fff000") `shouldBe` "<rect fill=\"#fff000\" x=\"0\" y=\"0\" width=\"1\" height=\"1\"/>"
        encodePixel (20, 50, "#e5e5e5") `shouldBe` "<rect fill=\"#e5e5e5\" x=\"20\" y=\"50\" width=\"1\" height=\"1\"/>"
  describe "encodeCanvas" $ it "encodes every pixel in the matrix and wraps it in a <svg/> tag" $ do
    encodeCanvas (0, 0) (2, 2) [(0, 0, "#ffffff"), (0, 1, "#000000"), (1, 0, "#000000"), (1, 1, "#ffffff")]
      `shouldBe` "<svg viewBox=\"0 0 2 2\" xmlns=\"http://www.w3.org/2000/svg\"><rect fill=\"#ffffff\" x=\"0\" y=\"0\" width=\"1\" height=\"1\"/><rect fill=\"#000000\" x=\"0\" y=\"1\" width=\"1\" height=\"1\"/><rect fill=\"#000000\" x=\"1\" y=\"0\" width=\"1\" height=\"1\"/><rect fill=\"#ffffff\" x=\"1\" y=\"1\" width=\"1\" height=\"1\"/></svg>"
