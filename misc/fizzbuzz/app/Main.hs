module Main where

import FizzBuzz

main :: IO ()
main = mapM_ putStrLn $ fizzbuzz [1..100]
