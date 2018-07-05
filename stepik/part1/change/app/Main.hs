module Main where

import           Change
import           Data.List

main :: IO ()
main = print $ nub . change $ 12
