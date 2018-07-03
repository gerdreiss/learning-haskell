module Perms
  ( perms
  , perms0
  ) where

perms0 :: [a] -> [[a]]
perms0 []     = [[]]
perms0 [a]    = [[a]]
perms0 [a, b] = [[a, b], [b, a]]
perms0 (x:xs) = concatMap insertions $ perms0 xs
  where
    insertion xs pos = take pos xs ++ [x] ++ drop pos xs
    insertions xs = map (insertion xs) [0 .. length xs]

perms :: [a] -> [[a]]
perms []     = [[]]
perms [x]    = [[x]]
perms (x:xs) = concatMap (insertElem x) (perms xs)
  where
    insertElem x []         = [[x]]
    insertElem x yss@(y:ys) = (x : yss) : map (y :) (insertElem x ys)
