module Fibonacci
  ( fibs
  ) where

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
