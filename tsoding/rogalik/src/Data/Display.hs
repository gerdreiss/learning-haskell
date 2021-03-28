module Data.Display where

import           Data.Array
import           Data.Coord

type Width = Int
type Height = Int

type Pixel = Char

data Display = Display
  { displayWidth  :: Width
  , displayHeight :: Height
  , displayPixels :: Array (Row, Col) Pixel
  }
  deriving Show

data Rect = Rect Row Col Width Height
