module Control.Display where

import           Data.Array
import           Data.Coord
import           Data.Display

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
  rect   = Rect 0 0 width height
  width  = displayWidth display
  height = displayHeight display

drawPixel :: Col -> Row -> Pixel -> Display -> Display
drawPixel x y = fillRect $ Rect x y 1 1

drawVLine :: Col -> Row -> Row -> Pixel -> Display -> Display
drawVLine col fromRow toRow =
  fillRect $ Rect col fromRow 1 (toRow - fromRow + 1)

drawHLine :: Col -> Col -> Row -> Pixel -> Display -> Display
drawHLine fromCol toCol row =
  fillRect $ Rect fromCol row (toCol - fromCol + 1) 1

drawRect :: Rect -> Pixel -> Display -> Display
drawRect (Rect x y w h) pixel =
  drawHLine x (x + w - 1) y pixel
    . drawHLine x (x + w - 1) (y + h - 1) pixel
    . drawVLine x           y (y + h - 1) pixel
    . drawVLine (x + w - 1) y (y + h - 1) pixel

fillRect :: Rect -> Pixel -> Display -> Display
fillRect (Rect x y w h) pixel display = display
  { displayPixels = displayPixels display // do
                      x <- [x .. (x + w - 1)]
                      y <- [y .. (y + h - 1)]
                      return ((coordX, coordY), pixel)
  }
 where
  width  = displayWidth display
  height = displayHeight display
  coordX = x `mod` width
  coordY = y `mod` height
