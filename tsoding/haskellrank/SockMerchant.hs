-- https://www.hackerrank.com/challenges/sock-merchant/problem

import Data.List

solution :: [Int] -> Int
solution = sum . map (\xs -> length xs `div` 2) . group . sort

main :: IO ()
main = interact $ show . solve . map read . tail . words

