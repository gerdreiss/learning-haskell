module Control.Room where

import qualified Data.Map                      as M

import           Data.Geom
import           Data.Item
import           Data.Room

mkRoom :: Rect -> Room
mkRoom rect = Room rect mempty

-- showRoom :: Room -> String
-- showRoom room =
--   let
--     cols = roomCols room
--     rows = roomRows room
--   in
--     unlines
--       [ [ itemCh row col | col <- [0 .. cols - 1] ] | row <- [0 .. rows - 1] ]
--   where itemCh r c = maybe '.' itemChar $ M.lookup (r, c) (roomItems room)

addItem :: Pos -> Item -> Room -> Room
addItem cell item room = room { roomItems = M.insert cell item items }
  where items = roomItems room

testRoom :: Room
testRoom =
  addItem (Pos 1 1) (WeaponItem Sword)
    . addItem (Pos 0 0) (GoldItem 69)
    $ mkRoom (Rect (Pos 0 0) (Size 10 20))

roomFloor :: Pixel
roomFloor = '.'
