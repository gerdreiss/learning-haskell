module EvenOnly where

evenOnly :: [a] -> [a]
evenOnly = (foldr (\(i,x) xs -> if (even i) then x:xs else xs) []) . (\xs -> zipWith (,) [1..(length xs)] xs)
