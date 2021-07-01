{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Picture.SVG
  ( encodePicture
  , encodePixel
  , Color
  , Pixel
  , Picture
  , PictureOptions(..)
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Foldable

type Color = ByteString
type Pixel = (Integer, Integer, Color)
type Picture = [Pixel]

data PictureOptions
  = PictureOptions
  { offsetX :: Integer
  , offsetY :: Integer
  , width :: Integer
  , height :: Integer
  , pixelSize :: Integer
  }

encodePicture :: PictureOptions -> Picture -> ByteString
encodePicture PictureOptions {..} matrix =
  "<svg viewBox=\""
    <> B.pack (show offsetX)
    <> " "
    <> B.pack (show offsetY)
    <> " "
    <> B.pack (show width)
    <> " "
    <> B.pack (show height)
    <> "\" xmlns=\"http://www.w3.org/2000/svg\">"
    <> (foldl' (<>) "" . map (encodePixel pixelSize)) matrix
    <> "</svg>"

encodePixel :: Integer -> (Integer, Integer, Color) -> ByteString
encodePixel pixelSize (x, y, color) =
  "<rect fill=\""
    <> color
    <> "\" x=\""
    <> B.pack (show x)
    <> "\" y=\""
    <> B.pack (show y)
    <> "\" width=\""
    <> B.pack (show pixelSize)
    <> "\" height=\""
    <> B.pack (show pixelSize)
    <> "\"/>"
