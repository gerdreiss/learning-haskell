module Triple where

data Triple a =
  Tr a a a
  deriving (Eq, Show)

instance Functor Triple where
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  pure a = Tr a a a
  Tr f g h <*> Tr a b c = Tr (f a) (g b) (h c)

-- my solution
-- instance Foldable Triple where
--   foldr f ini (Tr a b c) = f a . f b . f c $ ini
--   foldl f ini (Tr a b c) = f (f (f ini a) b) c

-- an unexpectedly elegant, and a litle cheaty, solution from somebody else ;)
instance Foldable Triple where
  foldr f ini (Tr a b c) = foldr f ini [a, b, c]
  foldl f ini (Tr a b c) = foldl f ini [a, b, c]
