module Data.Combinatorial
  ( Combination
  , Combinatorial
  , carthesian
  , combinatorial
  , permutations
  )
where

import Control.Monad (replicateM)
import Data.List (permutations)

type Combination a b c = [(a, b, c)]
type Combinatorial a b c = [Combination a b c]

carthesian :: [a] -> [b] -> [(a, b)]
carthesian xs ys = [ (x, y) | x <- xs, y <- ys ]

combinatorial :: [a] -> [b] -> [c] -> Combinatorial a b c
combinatorial as bs cs = combinate $ carthesian as bs
  where combinate xs = map (zipWith (\(x, y) c -> (x, y, c)) xs) (replicateM (length xs) cs)
