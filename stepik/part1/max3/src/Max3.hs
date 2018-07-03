module Max3
  ( max3
  ) where

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 [] [] []             = []
max3 (x:xs) (y:ys) (z:zs) = max x (max y z) : max3 xs ys zs
