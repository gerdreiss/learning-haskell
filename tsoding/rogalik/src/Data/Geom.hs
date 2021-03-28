module Data.Geom where

type Col = Int
type Row = Int

type Width = Int
type Height = Int

type Cols = Width
type Rows = Height

type Pos = (Row, Col)
type Size = (Height, Width)

type Pixel = Char

data Rect = Rect Row Col Width Height
  deriving Show
