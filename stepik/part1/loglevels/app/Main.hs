module Main where

import           LogLevels

main :: IO ()
main = print $ cmp Error Warning
