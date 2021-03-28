module Control.Display where

import qualified Data.Map                      as M

import           Control.Item
import           Control.Room
import           Data.Array
import           Data.Display
import           Data.Geom
import           Data.List
import           Data.Room

mkDisplay :: Width -> Height -> Pixel -> Display
mkDisplay width height pixel = Display width height pixels
 where
  pixels = array
    ((0, 0), (width - 1, height - 1))
    [ ((x, y), pixel) | x <- [0 .. width - 1], y <- [0 .. height - 1] ]

showDisplay :: Display -> String
showDisplay display = unlines
  [ [ pixels ! (x, y) | x <- [0 .. width - 1] ] | y <- [0 .. height - 1] ]
 where
  pixels = displayPixels display
  width  = displayWidth display
  height = displayHeight display

fillDisplay :: Pixel -> Display -> Display
fillDisplay pixel display = fillRect rect pixel display
 where
  rect   = Rect (Pos 0 0) (Size width height)
  width  = displayWidth display
  height = displayHeight display

fillRect :: Rect -> Pixel -> Display -> Display
fillRect (Rect (Pos rectX rectY) (Size rectW rectH)) pixel display = display
  { displayPixels = displayPixels display // do
                      x <- [rectX .. (rectX + rectW - 1)]
                      y <- [rectY .. (rectY + rectH - 1)]
                      return ((x `mod` width, y `mod` height), pixel)
  }
 where
  width  = displayWidth display
  height = displayHeight display

drawPixel :: Pos -> Pixel -> Display -> Display
drawPixel (Pos x y) = fillRect $ Rect (Pos x y) (Size 1 1)

drawVLine :: Pos -> Height -> Pixel -> Display -> Display
drawVLine (Pos x y) height =
  fillRect $ Rect (Pos x y) (Size 1 (height - y + 1))

drawHLine :: Pos -> Width -> Pixel -> Display -> Display
drawHLine (Pos x y) width = fillRect $ Rect (Pos x y) (Size (width - x + 1) 1)

drawRect :: Rect -> Pixel -> Display -> Display
drawRect (Rect (Pos x y) (Size w h)) pixel =
  drawHLine (Pos x y) width pixel
    . drawHLine (Pos x height) width pixel
    . drawVLine (Pos x y)     height pixel
    . drawVLine (Pos width y) height pixel
 where
  width  = x + w - 1
  height = y + h - 1

drawRoom :: Room -> Display -> Display
drawRoom room display =
  let items    = M.toList (roomItems room)
      rect     = roomRect room
      display' = fillRect rect roomFloor display
  in  foldl' folderF display' items
 where
  (Rect (Pos roomX roomY) _) = roomRect room
  folderF disp (Pos itemX itemY, item) =
    drawPixel (Pos (roomX + itemX) (roomY + itemY)) (itemChar item) disp
