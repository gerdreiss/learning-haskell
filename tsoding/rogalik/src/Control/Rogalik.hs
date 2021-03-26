module Control.Rogalik where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Rogalik


itemChar :: Item -> Char
itemChar (Gold _) = '*'
itemChar Coq      = 'D'

mkRoom :: Rows -> Cols -> Room
mkRoom rows cols = Room cols rows mempty

getRoom :: Index Room -> Rogalik -> Room
getRoom idx rogalik = rogalikRooms rogalik ! idx

addItem :: (Row, Col) -> Item -> Room -> Room
addItem cell item room = room { roomItems = M.insert cell item items }
  where items = roomItems room

showRoom :: Room -> String
showRoom room =
  let
    cols = roomCols room
    rows = roomRows room
  in
    unlines
      [ [ itemCh row col | col <- [0 .. cols - 1] ] | row <- [0 .. rows - 1] ]
  where itemCh r c = maybe '.' itemChar $ M.lookup (r, c) (roomItems room)

testRoom :: Room
testRoom = addItem (1, 1) Coq . addItem (0, 0) (Gold 69) $ mkRoom 10 20
