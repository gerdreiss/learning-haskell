module Control.Rogalik where

import qualified Data.Map                      as M

import           Control.Room
import           Data.Array
import           Data.Display
import           Data.Geom
import           Data.Rogalik
import           Data.Room


getRoom :: Index Room -> Rogalik -> Room
getRoom idx rogalik = rogalikRooms rogalik ! idx


generateRogalik :: Rogalik
generateRogalik = Rogalik
  { rogalikRooms  = array roomsIndexRange $ zip (range roomsIndexRange) rooms
  , rogalikPlayer = Player { playerRoom    = Index 0
                           , playerPos     = Pos 0 0
                           , playerGold    = 0
                           , playerWeapons = []
                           }
  }
 where
  roomsIndexRange = (Index 0, Index (roomsCount - 1))
  roomsCount      = 3
  rooms           = [mkRoom $ Rect (Pos 0 0) (Size 10 5)]


renderRogalik :: Rogalik -> Display -> Display
renderRogalik rogalik display = undefined
