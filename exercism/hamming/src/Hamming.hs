module Hamming
  (distance)
  where

distance :: String -> String -> Maybe Int
distance xs ys =
  if length xs /= length ys then Nothing
  else Just . length . filter id $ zipWith (/=) xs ys

distance0 :: String -> String -> Maybe Int
distance0 xs ys =
  if length xs /= length ys then Nothing
  else Just $ length $ filter (uncurry (/=)) (xs `zip` ys)
