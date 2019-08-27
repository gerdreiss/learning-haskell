module Main where

import           Control.Monad.State
import           RPS
import           System.Random

main :: IO ()
main = do
  g <- newStdGen
  let r = evalState (game 10) g
  print r
