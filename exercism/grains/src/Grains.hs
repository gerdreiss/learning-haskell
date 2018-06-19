module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n `elem` [1..64] = Just $ 2^(n-1)
  | otherwise        = Nothing

total :: Integer
total = maybe 0 id . fmap sum . sequence . map square $ [1..64]
-- or just (2 ^ 64) - 1
