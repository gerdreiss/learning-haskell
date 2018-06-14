module SumOfMultiples (sumOfMultiples) where

import           Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
    sum $ nub [n * f | f <- factors, n <- [1..limit], n * f < limit]
    -- sum $ filter (\x -> isFactor x factors) [1..limit-1]
    --     where isFactor x = any (\n -> x `mod` n == 0)

