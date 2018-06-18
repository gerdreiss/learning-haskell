module Main where

import           Lib

main :: IO ()
main = do
  print (sum'n'count 1123)
  print (getSecondFrom 1 "2" False)
