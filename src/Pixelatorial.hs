{-# LANGUAGE RecordWildCards #-}

module Pixelatorial
  ( module Codec.Picture.SVG
  , module Data.BigInt
  , module Data.Combinatorial
  , module Pixelatorial.Options
  , (?:)
  , pixelCombinations
  )
where

import Codec.Picture.SVG
import Data.BigInt
import Data.Combinatorial
import Data.List
import Data.Maybe
import Pixelatorial.Options

(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe

pixelCombinations :: PixelatorialOptions -> IO (Combinatorial Integer Integer String)
pixelCombinations PixelatorialOptions {..} = do
  colors <- lines <$> readFile colorSet
  let combinations = combinatorial [1 .. width] [1 .. height] colors
  let afterDrop    = genericDrop (skip ?: 0) combinations
  return $ maybe combinations (`genericTake` combinations) cycles
