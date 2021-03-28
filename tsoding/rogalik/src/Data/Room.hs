module Data.Room where

import qualified Data.Map                      as M

import           Data.Geom
import           Data.Item

data Room = Room
  { roomRect  :: Rect
  , roomItems :: M.Map Pos Item
  }
  deriving Show
