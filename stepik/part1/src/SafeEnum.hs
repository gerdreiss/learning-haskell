module SafeEnum where

class (Ord t, Enum t, Bounded t) => SafeEnum t where
    ssucc :: t -> t
    ssucc x = if x < maxBound then succ x else minBound
    spred :: t -> t
    spred x = if x > minBound then pred x else maxBound

instance SafeEnum Bool where
