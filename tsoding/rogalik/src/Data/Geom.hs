module Data.Geom where

import           Data.Array

type Col = Int
type Row = Int

type Width = Int
type Height = Int

type Pixel = Char

data Pos = Pos Col Row
  deriving (Eq, Ord, Ix, Show)

data Size = Size Width Height
  deriving (Eq, Ord, Ix, Show)

data Rect = Rect Pos Size
  deriving (Eq, Ord, Show)

instance Semigroup Size where
  (<>) (Size w1 h1) (Size w2 h2) = Size (w1 + w2) (h1 + h2)

instance Monoid Size where
  mempty = Size 0 0

(^+^) :: Size -> Size -> Size
(^+^) s1 s2 = s1 <> s2

(^-^) :: Size -> Size -> Size
(^-^) (Size w1 h1) (Size w2 h2) = Size (w1 - w2) (h1 - h2)
