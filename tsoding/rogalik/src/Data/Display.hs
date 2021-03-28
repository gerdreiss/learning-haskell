module Data.Display where

import           Data.Array
import           Data.Geom
import           Data.Room

data Display = Display
  { displayWidth  :: Width
  , displayHeight :: Height
  , displayPixels :: Array (Row, Col) Pixel
  }
  deriving Show
