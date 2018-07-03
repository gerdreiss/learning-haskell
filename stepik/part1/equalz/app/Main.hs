module Main where

import           Equals

main :: IO ()
main = print $ (1 :: Integer) === (1 :: Integer)
