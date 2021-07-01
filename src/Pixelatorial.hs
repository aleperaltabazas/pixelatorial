{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Pixelatorial
  ( module Codec.Picture.SVG
  , module Data.BigInt
  , module Data.Combinatorial
  , CanvasConfig(..)
  , (?:)
  , pixelCombinations
  )
where

import Codec.Picture.SVG
import Data.ByteString
import Data.BigInt
import Data.Combinatorial
import Data.Maybe

(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe

data CanvasConfig
  = CanvasConfig
  { colors :: [Color]
  , canvasWidth :: Integer
  , canvasHeight :: Integer
  , canvasPixelSize :: Integer
  } deriving (Show, Eq, Read)

pixelCombinations :: CanvasConfig -> Combinatorial Integer Integer ByteString
pixelCombinations CanvasConfig {..} =
  combinatorial [1, 1 + canvasPixelSize .. canvasWidth] [1, 1 + canvasPixelSize .. canvasHeight] colors
