module Arr where

newtype Arr2 e1 e2 a =
  Arr2
    { getArr2 :: e1 -> e2 -> a
    }

newtype Arr3 e1 e2 e3 a =
  Arr3
    { getArr3 :: e1 -> e2 -> e3 -> a
    }

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 g) = Arr2 (\e1 e2 -> f $ g e1 e2)

instance Functor (Arr3 e1 e2 e3) where
  fmap g (Arr3 f) = Arr3 (\e1 e2 e3 -> g $ f e1 e2 e3)
