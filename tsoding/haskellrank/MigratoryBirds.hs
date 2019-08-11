-- https://www.hackerrank.com/challenges/migratory-birds/problem

import Data.List
import Data.Function -- this is where the 'on' magic comes from

solution :: [Int] -> Int
solution = head . head . sortBy (flip compare `on` length) . group . sort

main :: IO ()
main = interact $ show . solution . map read . tail . words

