module Qsort (qsort) where

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort [x]    = [x]
qsort (x:xs) = ltx xs ++ x : eqx xs ++ gtx xs where
  ltx = qsort . filter (< x)
  gtx = qsort . filter (> x)
  eqx = filter (== x)
