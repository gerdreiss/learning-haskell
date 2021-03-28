module Data.Display where

import           Data.Array
import           Data.Geom
import           Data.Room

data Display = Display
  { displaySize   :: Size
  , displayPixels :: Array Pos Pixel
  }
  deriving Show
