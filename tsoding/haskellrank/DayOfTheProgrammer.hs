-- https://www.hackerrank.com/challenges/day-of-the-programmer/problem

import Text.Printf

leapDay :: Int -> String
leapYear = printf "12.09.%d"

normDay :: Int -> String
normDay = printf "13.09.%d"

julian :: Int -> String
julian year
  | year `mod` 4 == 0 = leapDay year
  | otherwise         = normDay year

gregorian :: Int -> String
gregorian year
    | year `mod` 400 == 0 = leapDay year
    | year `mod` 100 == 0 = normDay year
    | year `mod`   4 == 0 = leapDay year
    | otherwise           = normDay year

russian :: Int -> String
russian year
  | year <= 1917 = julian year
  | year == 1918 = "26.09.1918"
  | otherwise    = gregorian year

main = interact $ russian . read

