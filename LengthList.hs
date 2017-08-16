module LengthList where

lengthListWhere :: [a] -> Int
lengthListWhere = foldr f 0 where
   f _ s = s + 1

lengthListLambda :: [a] -> Int
lengthListLambda = foldr (\_ s -> s + 1) 0
