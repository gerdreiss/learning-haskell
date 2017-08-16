module SumOdd where

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if x `mod` 2 == 0 then s else x + s) 0
