module Change
  ( change
  , change0
  ) where

import Data.List

coins :: (Ord a, Num a) => [a]
coins = [2, 3, 5, 10]

change :: (Ord a, Num a) => a -> [[a]]
change 0   = [[]]
change sum = [sort (c : r) | c <- coins, c <= sum, r <- change (sum - c)]

change0 :: (Ord a, Num a) => a -> [[a]]
change0 n
  | n < 0     = []
  | n == 0    = [[]]
  | otherwise = concat [map (x :) (change0 (n - x)) | x <- coins, n - x >= 0]
