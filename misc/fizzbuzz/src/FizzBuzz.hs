module FizzBuzz
    ( fizzbuzz
    ) where

fizzbuzz :: (Show a, Integral a) => [a] -> [String]
fizzbuzz = map fizzbuzz1

fizzbuzz1 :: (Show a, Integral a) => a -> String
fizzbuzz1 i | i `mod` 15 == 0 = "FizzBuzz"
            | i `mod` 5 == 0 = "Fizz" 
            | i `mod` 3 == 0 = "Buzz"
            | otherwise = show i
