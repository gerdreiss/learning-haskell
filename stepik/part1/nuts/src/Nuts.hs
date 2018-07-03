module Nuts (Nat(..), toNat, fromNat, add, mul, fac) where

data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero    = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat (n - 1))

add :: Nat -> Nat -> Nat
add a b = toNat $ (fromNat a) + (fromNat b)

mul :: Nat -> Nat -> Nat
mul a b = toNat $ (fromNat a) * (fromNat b)

fac :: Nat -> Nat
fac n = toNat $ fac' $ fromNat n
  where
    fac' :: Integer -> Integer
    fac' 0 = 1
    fac' n = n * fac' (n - 1)


-- add :: Nat -> Nat -> Nat
-- add Zero    b       = b
-- add a       Zero    = a
-- add (Suc a) (Suc b) = add (Suc (Suc a)) b

-- mul :: Nat -> Nat -> Nat
-- mul (Suc Zero)  acc = acc
-- mul (Suc a)     acc = mul a (add acc y)
-- mul _           _   = Zero

-- fac :: Nat -> Nat
-- fac x = helper x x
--   where
--     helper (Zero)      _   = Suc Zero
--     helper (Suc Zero)  acc = acc
--     helper (Suc a) acc     = mul x (fac a)
