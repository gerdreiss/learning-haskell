module Data.Rogalik where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Geom
import           Data.Item
import           Data.Room

newtype Index a =
  Index Int
  deriving (Eq, Ord, Ix, Show)

data Player = Player
  { playerRoom    :: Index Room
  , playerPos     :: Pos
  , playerGold    :: Int
  , playerWeapons :: [Weapon]
  }
  deriving Show

data Rogalik = Rogalik
  { rogalikRooms  :: Array (Index Room) Room
  , rogalikPlayer :: Player
  }
  deriving Show

