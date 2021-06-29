module Lib
  ( someFunc
  , combinatorial
  , generateSVG
  , run
  , forIndexM_
  )
where

import Control.Monad
import Data.List
import System.Directory

someFunc :: IO ()
someFunc = putStrLn "someFunc"

f :: Integer -> Integer -> [String] -> IO ()
f height width colors = do
  let valued       = combinatorial height width colors
  let combinations = fromIntegral (length colors) ^ (height * width)
  putStrLn $ "Total combinations: " ++ show combinations
  forM_ valued $ \xs -> do
    forM_ xs $ \(x, y, v) -> putStrLn $ show (x, y) ++ ": " ++ show v
    putStrLn ""

combinatorial :: Integer -> Integer -> [a] -> [[(Integer, Integer, a)]]
combinatorial width height colors = do
  let baseMatrix = carthesian [1 .. width] [1 .. height]
  combinate baseMatrix
  where combinate xs = map (zipWith (\(x, y) c -> (x, y, c)) xs) (replicateM (length xs) colors)

carthesian xs ys = [ (x, y) | x <- xs, y <- ys ]

generateSVG :: [(Integer, Integer, String)] -> String
generateSVG matrix =
  "<svg viewBox=\"0 0 5 3\" xmlns=\"http://www.w3.org/2000/svg\" >" ++ (unwords . map pointToSVG) matrix ++ "</svg>"

pointToSVG :: (Integer, Integer, String) -> String
pointToSVG (x, y, color) =
  "<rect fill=\"" ++ color ++ "\" x=\"" ++ show (x - 1) ++ "\" y=\"" ++ show (y - 1) ++ "\" width=\"1\" height=\"1\"/>\n"

run matrices = do
  print $ length matrices
  let svgsFolder = "svgs"
  removePathForcibly svgsFolder
  createDirectory svgsFolder
  withCurrentDirectory svgsFolder $ forIndexM_ matrices $ \m i -> do
    let fileName = show i ++ ".svg"
    let svg      = generateSVG m
    writeFile fileName svg

forIndexM_ :: Monad m => [a] -> (a -> Integer -> m b) -> m ()
forIndexM_ ts f = void $ foldr applyF (return 0) ts
 where
  applyF e acc = do
    i <- acc
    f e i
    return (i + 1)

forEach :: Monad m => [a] -> (a -> m ()) -> m ()
forEach ts f = foldr (\e _ -> f e) (return ()) ts
