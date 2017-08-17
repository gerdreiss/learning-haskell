module MeanList where

meanList :: [Double] -> Double
meanList = (\(s, c) -> s/c) . foldr (\x (s, c) -> (s + x, c + 1)) (0, 0)
