module Queries where

import           Control.Applicative
import           Control.Monad       (guard)

_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = prop <$> vals

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
  val <- vals
  guard (test val)
  return val

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  guard (prop1 d1 == prop2 d2)
  return (d1, d2)

startsWith :: Char -> String -> Bool
startsWith char string = char == head string

startsWithS :: String -> String -> Bool
startsWithS prefix string
  | null prefix = True
  | length prefix > length string = False
  | otherwise = startsWith (head prefix) string && startsWithS (tail prefix) (tail string)
