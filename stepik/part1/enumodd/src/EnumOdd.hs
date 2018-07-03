module EnumOdd
  ( Odd(..)
  , Enum(..)
  ) where

newtype Odd =
  Odd Integer
  deriving (Eq, Show)

instance Enum Odd where
  toEnum i = Odd (toInteger i)
  fromEnum (Odd n) = fromEnum n
  succ (Odd n) = Odd (n + 2)
  pred (Odd n) = Odd (n - 2)
  enumFrom (Odd n) = map Odd [n,n + 2 ..]
  enumFromTo (Odd n) (Odd m) = map Odd [n,n + 2 .. m]
  enumFromThen (Odd n) (Odd n') = map Odd [n,n' ..]
  enumFromThenTo (Odd n) (Odd n') (Odd m) = map Odd [n,n' .. m]
--   succ (Odd x)     = Odd (toInteger $ fromIntegral(x) + 2)
--   pred (Odd x)     = Odd (toInteger $ fromIntegral(x) - 2)
--   toEnum n         = Odd (toInteger $ 2 * n + 1)
--   fromEnum (Odd x) = fromInteger $ (x - 1) `div` 2
