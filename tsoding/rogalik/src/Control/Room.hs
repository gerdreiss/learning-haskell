module Control.Room where

import qualified Data.Map                      as M

import           Data.Coord
import           Data.Item
import           Data.Room

mkRoom :: Rows -> Cols -> Room
mkRoom rows cols = Room cols rows mempty

-- showRoom :: Room -> String
-- showRoom room =
--   let
--     cols = roomCols room
--     rows = roomRows room
--   in
--     unlines
--       [ [ itemCh row col | col <- [0 .. cols - 1] ] | row <- [0 .. rows - 1] ]
--   where itemCh r c = maybe '.' itemChar $ M.lookup (r, c) (roomItems room)

addItem :: (Row, Col) -> Item -> Room -> Room
addItem cell item room = room { roomItems = M.insert cell item items }
  where items = roomItems room

testRoom :: Room
testRoom =
  addItem (1, 1) (ItemWeapon Sword) . addItem (0, 0) (GoldItem 69) $ mkRoom 10
                                                                            20
