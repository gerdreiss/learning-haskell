module CollatzConjecture
  ( collatz
  ) where

collatz :: Integer -> Maybe Integer
collatz n
  | n > 0     = Just . fromIntegral . length . takeWhile (> 1) $ iterate next n
  | otherwise = Nothing

next :: Integer -> Integer
next n | even n    = n `div` 2
       | otherwise = n * 3 + 1


-- my ugly solution
collatz' :: Integer -> Maybe Integer
collatz' x = if x < 1
        then Nothing
        else steps x (Just 0)

steps :: Integer -> Maybe Integer -> Maybe Integer
steps 1 cnt = cnt
steps x cnt = if x `mod` 2 == 0
        then steps (x `quot` 2) (fmap (+1) cnt)
        else steps (x * 3 + 1) (fmap (+1) cnt)


-- beautiful solution, not mine:
collatz'' :: Integer -> Maybe Integer
collatz'' n
    | n <= 0         = Nothing
    | n == 1         = Just 0
    | n `mod` 2 == 0 = fmap (+1) $ collatz'' (n `div` 2)
    | otherwise      = fmap (+1) $ collatz'' (n * 3 + 1)

-- better, but still inelegant
collatz''' :: Integer -> Maybe Integer
collatz''' n | n <= 0    = Nothing
        | n == 1    = Just 0
        | otherwise = fmap (+1) . collatz''' . next $ n

