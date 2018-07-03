module Main where

import           Loggers

main :: IO ()
main = print (toLogger id "ze logger" 1)
