module RevRange where

revRange :: (Char, Char) -> [Char]
revRange = unfoldr g where
    g (x, y) | x <= y    = Just (y, (x, pred y))
             | otherwise = Nothing
