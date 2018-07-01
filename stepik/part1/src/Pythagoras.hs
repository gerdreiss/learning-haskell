module Pythagoras
  ( pythagoreanTriple
  ) where

import           Control.Monad

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
  c <- [1 .. x]
  b <- [1 .. c]
  a <- [1 .. b]
  guard $ a ^ 2 + b ^ 2 == c ^ 2
  return (a, b, c)

pythagoreanTriple' :: Int -> [(Int, Int, Int)]
pythagoreanTriple' x
  | x < 3 = []
  | otherwise =
    [(x, y, z) | x <- xs, y <- xs, z <- xs, x^2 + y^2 == z^2, x <= y]
  where
    xs = [3 .. x]
