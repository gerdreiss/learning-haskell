module Lists
    ( reverse
    , isPalindrome
    , isPalindrome0
    , isPalindrome1
    , sum3
    , groupElems
    ) where

import           Data.List     hiding (reverse)
import           Prelude       hiding (reverse)

import           Control.Arrow

reverse :: [a] -> [a]
reverse l = rev l []
    where
    rev xs a = foldl (flip (:)) a xs
    -- rev [] a     = a
    -- rev (x:xs) a = rev xs (x : a)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []     = True
isPalindrome [_]    = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

isPalindrome0 :: Eq a => [a] -> Bool
isPalindrome0 xs = xs == reverse xs

isPalindrome1 :: Eq a => [a] -> Bool
isPalindrome1 = uncurry (==) . (id &&& reverse)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = map sum $ transpose [xs, ys, zs]

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) =
    (x : takeWhile (== x) xs) : groupElems (dropWhile (== x) xs)
