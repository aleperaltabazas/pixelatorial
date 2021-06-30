module Data.BigInt
  ( BigInt(..)
  , incr
  )
where

-- The only use for this is to name the files as we iterate through the combinations
-- but not using all the memory 

import Data.Char
import Data.Foldable

newtype BigInt
  = BigInt
  { unBigInt :: [Int]
  } deriving (Eq)

instance Show BigInt where
  show (BigInt b) = foldl' (\acc e -> show e ++ acc) "" b

instance Read BigInt where
  readsPrec _ s = [ (BigInt (foldl' (\acc e -> read [e] : acc) [] s), "") | all isDigit s ]

incr :: BigInt -> BigInt
incr (BigInt xs) = BigInt $ go 1 xs
 where
  go n []       = [ n | n /= 0 ]
  go n (x : xs) = let x' = x + n in if x' == 10 then 0 : go 1 xs else x' : go 0 xs
