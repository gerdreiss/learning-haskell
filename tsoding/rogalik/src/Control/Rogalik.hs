module Control.Rogalik where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Rogalik

-- | Room functions

mkRoom :: Rows -> Cols -> Room
mkRoom rows cols = Room cols rows mempty

showRoom :: Room -> String
showRoom room =
  let
    cols = roomCols room
    rows = roomRows room
  in
    unlines
      [ [ itemCh row col | col <- [0 .. cols - 1] ] | row <- [0 .. rows - 1] ]
  where itemCh r c = maybe '.' itemChar $ M.lookup (r, c) (roomItems room)

itemChar :: Item -> Char
itemChar (Gold       _    ) = '*'
itemChar (ItemWeapon Sword) = '/'

addItem :: (Row, Col) -> Item -> Room -> Room
addItem cell item room = room { roomItems = M.insert cell item items }
  where items = roomItems room

testRoom :: Room
testRoom =
  addItem (1, 1) (ItemWeapon Sword) . addItem (0, 0) (Gold 69) $ mkRoom 10 20

-- | Display Functions

mkDisplay :: Width -> Height -> Display
mkDisplay width height = Display width height pixels
 where
  pixels = array
    ((0, 0), (width - 1, height - 1))
    [ ((x, y), ' ') | x <- [0 .. width - 1], y <- [0 .. height - 1] ]

showDisplay :: Display -> String
showDisplay display = ""

-- | Rogalik functions

getRoom :: Index Room -> Rogalik -> Room
getRoom idx rogalik = rogalikRooms rogalik ! idx

