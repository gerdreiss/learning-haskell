module SumNCount
  ( sum'n'count
  , getSecondFrom
  ) where

-- Chapter 1, Exercise 3
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (sumS s, lengthS s)
  where
    s = str x

sumS :: String -> Integer
sumS s = sum (map (\c -> read [c] :: Integer) s)

lengthS :: String -> Integer
lengthS s = toInteger $ length s

str :: Integer -> String
str i = show $ abs i

-- Chapter 2, Exercise 1
getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom _ y _ = y
