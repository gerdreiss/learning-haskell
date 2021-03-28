module Data.Rogalik where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Coord
import           Data.Item
import           Data.Room


newtype Index a =
  Index Int
  deriving (Eq, Ord, Ix, Show)

data Player = Player
  { playerRoom    :: Index Room
  , playerRow     :: Row
  , playerCol     :: Col
  , playerGold    :: Int
  , playerWeapons :: [Weapon]
  }

data Rogalik = Rogalik
  { rogalikRooms  :: Array (Index Room) Room
  , rogalikPlayer :: Player
  }

