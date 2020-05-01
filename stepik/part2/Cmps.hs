{-# LANGUAGE TypeOperators #-}

module Cmps where

import           Data.Monoid

infixr 9 |.|

newtype (|.|) f g a =
  Cmps2
    { getCmps2 :: f (g a)
    }
  deriving (Eq, Show)

newtype Cmps3 f g h a =
  Cmps3
    { getCmps3 :: f (g (h a))
    }
  deriving (Eq, Show)

{-
    x :: f (g a)
    phi :: g a -> g b
    fmap phi x :: f (g b)
    fmap h :: g a -> g b
-}
instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps2 x) = Cmps2 $ fmap (fmap h) x

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap h (Cmps3 x) = Cmps3 $ fmap (fmap (fmap h)) x

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldMap f (Cmps2 x) = foldMap (foldMap f) x
--
-- not mine, but beautiful
-- instance (Foldable f, Foldable g) => Foldable (f |.| g) where
--   foldMap = (. getCmps) . foldMap . foldMap
--
-- also not bad, actually my first thought
-- instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
--   foldr f ini (Cmps x) = appEndo (foldMap (foldMap (Endo . f)) x) ini
--
-- I like that one, too
-- instance (Foldable f, Foldable g) => Foldable (f |.| g) where
--   foldr f ini (Cmps x) = foldr (flip $ foldr f) ini x
