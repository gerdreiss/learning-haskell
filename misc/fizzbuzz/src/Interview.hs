module Interview where

plus1 :: Int -> Int
plus1 = (+ 1)

functor1 :: (Functor m) => (a -> b) -> m a -> m b
functor1 f fa = f <$> fa

applicative1 :: (Applicative m) => m (a -> b) -> m a -> m b
applicative1 mf ma = mf <*> ma


q10 :: (Traversable t, Applicative f) => t (f a, b) -> f (t (a, b))
q10 x = undefined
