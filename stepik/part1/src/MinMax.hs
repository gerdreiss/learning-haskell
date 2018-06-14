module MinMax
  ( minimum
  , maximum
  , reverse
  , isPalindrome
  , sum3
  , groupElems
  ) where

import           Control.Arrow
import           Data.List     hiding (maximum, minimum, reverse)
import           Prelude       hiding (maximum, minimum, reverse)



minimum :: (Ord a) => [a] -> a
minimum []     = error "Empty list"
minimum [x]    = x
minimum (x1:x2:xs)
  | x1 < x2   = minimum (x1:xs)
  | otherwise = minimum (x2:xs)


maximum :: (Ord a) => [a] -> a
maximum []     = error "Empty list"
maximum [x]    = x
maximum (x1:x2:xs)
  | x1 < x2   = maximum (x1:xs)
  | otherwise = maximum (x2:xs)


reverse :: [a] -> [a]
reverse l = rev l [] where
  rev []     a = a
  rev (x:xs) a = rev xs (x:a)


isPalindrome :: Eq a => [a] -> Bool
isPalindrome     [] = True
isPalindrome    [_] = True
isPalindrome (x:xs) = (x == last xs) && (isPalindrome $ init xs)


isPalindrome0 :: Eq a => [a] -> Bool
isPalindrome0 xs = xs == reverse xs


isPalindrome1 :: Eq a => [a] -> Bool
isPalindrome1 = uncurry (==) . (id &&& reverse)


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = map sum $ transpose [xs, ys, zs]


groupElems :: Eq a => [a] -> [[a]]
groupElems []     = []
groupElems (x:xs) = (x : takeWhile (==x) xs) : (groupElems $ dropWhile (==x) xs)
