{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad (void)
import Data.List (genericDrop, genericTake)
import Data.Foldable (foldl')
import Pixelatorial
import Pixelatorial.Options
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  opts@PixelatorialOptions { skip = offset, outputFolder = outputFolder, cycles = cycles } <- parsePixelatorialOptions
  canvasConfig@CanvasConfig {..} <- makeCanvasConfig opts
  let combinations = pixelCombinations canvasConfig
  let fileName     = outputFolder ?: "svgs"
  createDirectoryIfMissing True fileName
  let
    pictureOptions =
      PictureOptions { offsetX = 1, offsetY = 1, pixelSize = canvasPixelSize, width = canvasWidth, height = canvasHeight }
  let afterDrop = takeMaybe cycles . dropMaybe offset $ combinations
  forEach combinations $ \num comb -> writeFile (fileName ++ "/" ++ show num ++ ".svg") $ encodePicture pictureOptions comb

 where
  takeMaybe cycles it = maybe it (`genericTake` it) cycles
  dropMaybe offset it = maybe it (`genericDrop` it) offset

forEach :: Monad m => [a] -> (BigInt -> a -> m b) -> m ()
forEach xs f = void $ foldl' applyIteration (return $ BigInt [0]) xs
 where
  applyIteration n e = do
    num <- n
    f num e
    return $ incr num
