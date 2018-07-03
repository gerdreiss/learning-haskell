module Shopping
  ( Writer(..)
  , Shopping(..)
  , purchase
  , total
  , shopping1
  ) where

import           Control.Monad (ap, liftM)
import           Data.Monoid

newtype Writer w a = Writer {runWriter :: (a, w)} deriving Show

type Shopping = Writer ((Sum Integer), ([String])) ()

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  m >>= k =
    let (x, u) = runWriter m
        (y, v) = runWriter $ k x
    in Writer (y, u `mappend` v)

instance (Monoid w) => Applicative (Writer w) where
  pure  = return
  (<*>) = ap

instance (Monoid w) => Functor (Writer w) where
  fmap = liftM

writer :: (a, w) -> Writer w a
writer = Writer

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

purchase :: String -> Integer -> Shopping
purchase item cost = writer ((), ((Sum cost), (item:[])))

total :: Shopping -> Integer
total = getSum . fst . execWriter

items :: Shopping -> [String]
items = snd . execWriter

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328


