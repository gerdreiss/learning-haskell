{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.List

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
  xs <- do
    xs <- sort . map read . lines <$> readFile "./data/input.txt"
    return $ [0] ++ xs ++ [last xs + 3]
  let dp = 1 : map
        (\x ->
          sum
            $ map snd
            $ takeWhile (\(y, _) -> x > y)
            $ dropWhile (\(y, _) -> x - y > 3)
            $ zip xs dp
        )
        (tail xs)
  print $ last dp
