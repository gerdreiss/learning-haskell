module Queries where

import           Control.Monad (guard)

--_select :: (a -> b) -> [a] -> [b]
--_select prop vals = do
--   val <- vals
--   return (prop val)
_select :: (a -> b) -> [a] -> [b]
_select prop vals = prop <$> vals

--_where :: (a -> Bool) -> [a] -> [a]
--_where test vals = do
--  val <- vals
--  guard (test val)
--  return val
_where :: (a -> Bool) -> [a] -> [a]
_where test vals = [val | val <- vals, test val]

startsWith :: Char -> String -> Bool
startsWith char string = char == head string

startsWithS :: String -> String -> Bool
startsWithS prefix string
  | null prefix = True
  | length prefix > length string = False
  | otherwise = startsWith (head prefix) string && startsWithS (tail prefix) (tail string)

--_join0 :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
--_join0 data1 data2 prop1 prop2 = do
--  d1 <- data1
--  d2 <- data2
--  let dpairs = (d1, d2)
--  guard ((prop1 . fst $ dpairs) == (prop2 . snd $ dpairs))
--  return dpairs
_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join data1 data2 prop1 prop2 = [(d1, d2) | d1 <- data1, d2 <- data2, prop1 d1 == prop2 d2]
