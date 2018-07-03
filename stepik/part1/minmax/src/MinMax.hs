module MinMax
  ( minimum
  , maximum
  ) where

import           Data.List hiding (maximum, minimum)
import           Prelude   hiding (maximum, minimum)

minimum :: (Ord a) => [a] -> a
minimum [] = error "Empty list"
minimum [x] = x
minimum (x1:x2:xs)
  | x1 < x2 = minimum (x1 : xs)
  | otherwise = minimum (x2 : xs)

maximum :: (Ord a) => [a] -> a
maximum [] = error "Empty list"
maximum [x] = x
maximum (x1:x2:xs)
  | x1 < x2 = maximum (x1 : xs)
  | otherwise = maximum (x2 : xs)

