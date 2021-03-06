{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import Data.List (genericDrop, genericTake)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Pixelatorial
import Pixelatorial.Options
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  opts@PixelatorialOptions { skip = offset, outputFolder = outputFolder, cycles = cycles } <- parsePixelatorialOptions
  canvasConfig@CanvasConfig {..} <- makeCanvasConfig opts
  let fileName = outputFolder ?: "svgs"
  createDirectoryIfMissing True fileName
  let
    pictureOptions =
      PictureOptions { offsetX = 1, offsetY = 1, pixelSize = canvasPixelSize, width = canvasWidth, height = canvasHeight }
  start <- getPOSIXTime
  let combinations = pixelCombinations canvasConfig
  totalIterations <- forEach (takeMaybe cycles . dropMaybe offset $ combinations)
    $ \num comb -> BS.writeFile (fileName ++ "/" ++ show num ++ ".svg") $ encodePicture pictureOptions comb
  end <- getPOSIXTime
  putStrLn $ "Generated " ++ show totalIterations ++ " " ++ show canvasWidth ++ "x" ++ show canvasHeight ++ " images in " ++ show
    (end - start)

 where
  takeMaybe cycles it = maybe it (`genericTake` it) cycles
  dropMaybe offset it = maybe it (`genericDrop` it) offset

forEach :: Monad m => [a] -> (BigInt -> a -> m b) -> m BigInt
forEach xs f = foldl' applyIteration (return $ BigInt [0]) xs
 where
  applyIteration n e = do
    num <- n
    f num e
    return $ incr num
