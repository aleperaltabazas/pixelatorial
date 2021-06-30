{-# LANGUAGE RecordWildCards #-}

module Codec.Picture.SVG
  ( encodePicture
  , encodePixel
  , Color
  , Pixel
  , Picture
  , PictureOptions(..)
  , Width
  , Height
  )
where

import Data.Foldable

type Color = String
type Width = Integer
type Height = Integer
type Pixel = (Integer, Integer, Color)
type Picture = [Pixel]
type OffsetX = Int
type OffsetY = Int

data PictureOptions
  = PictureOptions
  { offsetX :: Integer
  , offsetY :: Integer
  , width :: Integer
  , height :: Integer
  , pixelSize :: Integer
  }

encodePicture :: PictureOptions -> Picture -> String
encodePicture PictureOptions {..} matrix =
  "<svg viewBox=\""
    ++ show offsetX
    ++ " "
    ++ show offsetY
    ++ " "
    ++ show width
    ++ " "
    ++ show height
    ++ "\" xmlns=\"http://www.w3.org/2000/svg\">"
    ++ (foldl' (++) "" . map (encodePixel pixelSize)) matrix
    ++ "</svg>"

encodePixel :: Integer -> Pixel -> String
encodePixel pixelSize (x, y, color) =
  "<rect fill=\""
    ++ color
    ++ "\" x=\""
    ++ show x
    ++ "\" y=\""
    ++ show y
    ++ "\" width=\""
    ++ show pixelSize
    ++ "\" height=\""
    ++ show pixelSize
    ++ "\"/>"
