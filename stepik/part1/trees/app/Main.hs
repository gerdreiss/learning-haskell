module Main where

import           Trees

main :: IO ()
main = print $ size $ Node (Leaf "a") (Leaf "b")
