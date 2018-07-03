module Main where

import           PersonParser

main :: IO ()
main = print $ parsePerson "firstName = G\nlastName = R\nage = 50"
