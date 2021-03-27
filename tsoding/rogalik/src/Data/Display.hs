module Data.Display where

import           Data.Array

type Width = Int
type Height = Int

data Display = Display
  { displayWidth  :: Width
  , displayHeight :: Height
  , displayPixels :: Array (Int, Int) Char
  }

mkDisplay :: Width -> Height -> Display
mkDisplay width height = Display width height pixels
 where
  pixels = array
    ((0, 0), (width - 1, height - 1))
    [ ((x, y), ' ') | x <- [0 .. width - 1], y <- [0 .. height - 1] ]

