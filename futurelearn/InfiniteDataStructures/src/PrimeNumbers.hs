module PrimeNumbers
  ( primes
  ) where

properfactors :: Int -> [Int]
properfactors x = filter (\y -> (x `mod` y == 0)) [2..(x - 1)]

numproperfactors :: Int -> Int
numproperfactors x = length (properfactors x)

primes :: [Int]
primes = filter (\x -> (numproperfactors x == 0)) [2..]
