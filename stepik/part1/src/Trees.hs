module Trees (Tree(..), height, size) where

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _)   = 0
height (Node l r) = max (1 + height l) (1 + height r)

size :: Tree a -> Int
size (Leaf _)   = 1
size (Node l r) = 1 + size l + size r


instance (Num a, Num b) => Num (a, b) where
    (+) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf n) = (1, n)
    go (Node left right) =
        let l = go left
            r = go right
        in (fst l + fst r, snd l + snd r)
        -- using the instance (Num a, Num b) above:
        -- go l + go r
        -- also nice:
        -- (\(n1, x1) (n2, x2) -> (n1 + n2, x1 + x2)) (go l) (go r)
