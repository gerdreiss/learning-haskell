module Data.Rogalik where

import           Data.Array
import qualified Data.Map                      as M

type Rows = Int
type Row = Int
type Cols = Int
type Col = Int
type Width = Int
type Height = Int
newtype Index a =
  Index Int
  deriving (Eq, Ord, Ix, Show)

data Weapon = Sword
  deriving Show

data Item
  = Gold Int
  | ItemWeapon Weapon
  deriving (Show)

data Room = Room
  { roomCols  :: Cols
  , roomRows  :: Rows
  , roomItems :: M.Map (Row, Col) Item
  }
  deriving Show

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

data Display = Display
  { displayWidth  :: Width
  , displayHeight :: Height
  , displayPixels :: Array (Int, Int) Char
  }
  deriving Show
