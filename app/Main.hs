{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Foldable
import Pixelatorial
import System.Directory

main :: IO ()
main = do
  opts@PixelatorialOptions {..} <- parsePixelatorialOptions
  combinations                  <- pixelCombinations opts
  let fileName = outputFolder ?: "svgs"
  createDirectoryIfMissing True fileName
  forEach combinations $ \num comb -> writeFile (fileName ++ "/" ++ show num ++ ".svg") $ encodeCanvas (1, 1) (width, height) comb

forEach :: Monad m => [a] -> (BigInt -> a -> m b) -> m ()
forEach xs f = void $ foldl' applyIteration (return $ BigInt [0]) xs
 where
  applyIteration n e = do
    num <- n
    f num e
    return $ incr num
