module Data.Geom where

type Col = Int
type Row = Int

type Width = Int
type Height = Int

type Pixel = Char

data Pos = Pos Row Col
  deriving (Eq, Ord, Show)

data Size = Size Height Width
  deriving (Eq, Ord, Show)

data Rect = Rect Pos Size
  deriving (Eq, Ord, Show)
