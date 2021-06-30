module Codec.Picture.SVG
  ( encodeCanvas
  , encodePixel
  , Color
  , Pixel
  , Canvas
  , Width
  , Height
  )
where

import Data.Foldable

type Color = String
type Width = Integer
type Height = Integer
type Pixel = (Integer, Integer, Color)
type Canvas = [Pixel]
type OffsetX = Int
type OffsetY = Int

encodeCanvas :: (OffsetX, OffsetY) -> (Width, Height) -> Canvas -> String
encodeCanvas (offsetX, offsetY) (width, height) matrix =
  "<svg viewBox=\""
    ++ show offsetX
    ++ " "
    ++ show offsetY
    ++ " "
    ++ show width
    ++ " "
    ++ show height
    ++ "\" xmlns=\"http://www.w3.org/2000/svg\">"
    ++ (foldl' (++) "" . map encodePixel) matrix
    ++ "</svg>"

encodePixel :: Pixel -> String
encodePixel (x, y, color) =
  "<rect fill=\"" ++ color ++ "\" x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" width=\"1\" height=\"1\"/>"
