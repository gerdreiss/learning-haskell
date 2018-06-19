module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n `elem` [1..64] = Just $ 2^(n-1)
  | otherwise        = Nothing

total :: Integer
total = maybe 0 id . fmap sum . sequence . map square $ [1..64]

-- better:
-- import Data.Maybe (fromJust)
-- total = fromJust $ sum <$> traverse square [1..64]

-- or
-- import Data.Maybe (mapMaybe)
-- total = sum $ mapMaybe square [1..64]

-- or totally simple
-- total = (2 ^ 64) - 1
