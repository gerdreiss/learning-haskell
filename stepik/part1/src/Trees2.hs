module Trees2 (Tree2(..)) where

data Tree2 a = Leaf (Maybe a) | Branch (Tree2 a) (Maybe a) (Tree2 a) deriving Show

instance Functor Tree2 where
    fmap f (Leaf (Just a))       = Leaf $ Just $ f a
    fmap f (Leaf Nothing)        = Leaf Nothing
    fmap f (Branch l (Just a) r) = Branch (fmap f l) (Just $ f a) (fmap f r)
    fmap f (Branch l Nothing r)  = Branch (fmap f l) Nothing (fmap f r)
