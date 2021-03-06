module Equals where

class Equals a where
    (===) :: a -> a -> Bool
    (=/=) :: a -> a -> Bool
    x =/= y = not (x === y)
    x === y = not (x =/= y)

instance Equals Bool where
    True === True = True
    False === False = True
    _ === _ = False

instance Equals Integer where
    (===) x y = x == y

instance (Equals a, Equals b) => Equals (a, b) where
    p1 === p2 = fst p1 === fst p2 && snd p1 === snd p2

class (Equals a) => Order a where
    lt_, le_, ge_, gt_ :: a -> a -> Bool
    max, min :: a -> a -> a
    compare :: a -> a -> Ordering
