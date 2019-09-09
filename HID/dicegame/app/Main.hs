module Main where

import Control.Monad.RWS
import Lib
import System.Random

main :: IO ()
main = newStdGen >>= print . evalRWS diceGame (1, 6)
