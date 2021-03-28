module Data.Room where

import qualified Data.Map                      as M

import           Data.Coord
import           Data.Item

type Rows = Int
type Cols = Int

data Room = Room
  { roomCols  :: Cols
  , roomRows  :: Rows
  , roomItems :: M.Map (Row, Col) Item
  }
  deriving Show
