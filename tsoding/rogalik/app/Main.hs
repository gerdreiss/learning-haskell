module Main where

import           Control.Display
import           Control.Rogalik
import           Data.Geom

main :: IO ()
main = do
    let display = mkDisplay (Size 80 30) ' '
        rogalik = generateRogalik 
     in putStrLn . renderDisplay $ renderRogalik rogalik display
  
