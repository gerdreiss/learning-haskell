module Main where

import           Expressions

main :: IO ()
main = print $ expand $ Val 1
