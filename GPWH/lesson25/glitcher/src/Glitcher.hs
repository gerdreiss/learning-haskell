module Glitcher
  ( glitch
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

glitch :: String -> IO ()
glitch fileName = do
  imageFile <- BC.readFile fileName
  let glitched = imageFile
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"
