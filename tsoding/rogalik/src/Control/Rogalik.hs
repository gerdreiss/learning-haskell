module Control.Rogalik where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Rogalik


itemChar :: Item -> Char
itemChar (Gold _) = '*'
itemChar Coq      = 'D'

mkRoom :: Rows -> Cols -> Room
mkRoom rows cols = Room cols rows M.empty

getRoom :: Index Room -> Rogalik -> Room
getRoom idx rogalik = rogalikRooms rogalik ! idx

addItem :: (Row, Col) -> Item -> Room -> Room
addItem cell item room = room { roomItems = M.insert cell item items }
  where items = roomItems room

showRoom :: Room -> String
showRoom room =
  let cols  = roomCols room
      rows  = roomRows room
      items = roomItems room
  in  unlines
        $ [ [ maybe '.' itemChar $ M.lookup (row, col) items
            | col <- [0 .. cols - 1]
            ]
          | row <- [0 .. rows - 1]
          ]

testRoom :: Room
testRoom = addItem (1, 1) Coq . addItem (0, 0) (Gold 69) $ mkRoom 10 20
