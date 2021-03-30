module Control.Rogalik where

import qualified Data.Map                      as M

import           Control.Display
import           Control.Room
import           Data.Array
import           Data.Display
import           Data.Foldable
import           Data.Geom
import           Data.Rogalik
import           Data.Room

-- getRoom :: Index Room -> Rogalik -> Room
-- getRoom idx rogalik = rogalikRooms rogalik ! idx

generateRogalik :: Rogalik
generateRogalik = Rogalik
  { rogalikRooms  = array roomsIndexRange $ zip (range roomsIndexRange) rooms
  , rogalikPlayer = Player { playerRoom    = Index 1
                           , playerPos     = Pos 0 0
                           , playerGold    = 0
                           , playerWeapons = []
                           }
  }
 where
  roomsIndexRange = (Index 0, Index (roomsCount - 1))
  rooms =
    [ mkRoom $ Rect (Pos 0 0) (Size 10 5)
    , mkRoom $ Rect (Pos 0 20) (Size 5 7)
    , mkRoom $ Rect (Pos 20 20) (Size 10 10)
    ]
  roomsCount = length rooms


renderPlayer :: Rogalik -> Display -> Display
renderPlayer rogalik = drawPixel playerScreenPos playerPixel
 where
  playerScreenPos = playerRoomPos <> playerPos player
  playerRoomPos =
    let Rect (Pos x y) _ = roomRect (rooms ! playerRoom player) in Pos x y
  player      = rogalikPlayer rogalik
  rooms       = rogalikRooms rogalik
  playerPixel = '@'


renderRooms :: Rogalik -> Display -> Display
renderRooms rogalik display = foldl' draw display rooms
 where
  draw  = flip drawRoom
  rooms = elems (rogalikRooms rogalik)


renderRogalik :: Rogalik -> Display -> Display
renderRogalik rogalik = renderPlayer rogalik . renderRooms rogalik
