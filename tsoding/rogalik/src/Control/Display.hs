module Control.Display where

import qualified Data.Map                      as M

import           Control.Item
import           Control.Room
import           Data.Array
import           Data.Display
import           Data.Geom
import           Data.List
import           Data.Room

mkDisplay :: Size -> Pixel -> Display
mkDisplay size pixel = Display size pixels
 where
  pixels = array
    range
    [ (Pos x y, pixel) | x <- [0 .. width - 1], y <- [0 .. height - 1] ]
  range               = (Pos 0 0, Pos (width - 1) (height - 1))
  (Size width height) = size

showDisplay :: Display -> String
showDisplay (Display size@(Size width height) pixels) = unlines
  [ [ pixels ! Pos x y | x <- [0 .. width - 1] ] | y <- [0 .. height - 1] ]

fillDisplay :: Pixel -> Display -> Display
fillDisplay pixel display@(Display size _) = fillRect rect pixel display
  where rect = Rect (Pos 0 0) size

fillRect :: Rect -> Pixel -> Display -> Display
fillRect rect pixel display = display
  { displayPixels = pixels // do
                      x <- [rectX .. (rectX + rectW - 1)]
                      y <- [rectY .. (rectY + rectH - 1)]
                      return (Pos (x `mod` width) (y `mod` height), pixel)
  }
 where
  (Rect    (Pos  rectX rectY ) (Size rectW rectH)) = rect
  (Display (Size width height) pixels            ) = display

drawPixel :: Pos -> Pixel -> Display -> Display
drawPixel (Pos x y) = fillRect $ Rect (Pos x y) (Size 1 1)

drawVLine :: Pos -> Height -> Pixel -> Display -> Display
drawVLine (Pos x y) height =
  fillRect $ Rect (Pos x y) (Size 1 (height - y + 1))

drawHLine :: Pos -> Width -> Pixel -> Display -> Display
drawHLine (Pos x y) width = fillRect $ Rect (Pos x y) (Size (width - x + 1) 1)

drawRect :: Rect -> Pixel -> Display -> Display
drawRect rect pixel =
  drawHLine (Pos x y) width pixel
    . drawHLine (Pos x height) width pixel
    . drawVLine (Pos x y)     height pixel
    . drawVLine (Pos width y) height pixel
 where
  (Rect (Pos x y) (Size w h)) = rect
  width                       = x + w - 1
  height                      = y + h - 1

drawRoom :: Room -> Display -> Display
drawRoom room display =
  let items    = M.toList (roomItems room)
      display' = fillRect rect roomFloor display
  in  foldl' folderF display' items
 where
  rect@(Rect (Pos roomX roomY) _) = roomRect room
  folderF disp (Pos itemX itemY, item) =
    drawPixel (Pos (roomX + itemX) (roomY + itemY)) (itemChar item) disp
