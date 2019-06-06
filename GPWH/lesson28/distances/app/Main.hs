module Main where

import qualified Data.Map as Map

import           Dist

main :: IO ()
main = do
  putStrLn "Enter the starting city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter the destination city name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance
--  putStrLn "Enter a username, gamerId and score"
--  user <- User <$> getLine <*> readInt <*> readInt
--  print user
--  putStrLn "Enter three numbers"
--  minInt <- minOfInts
--  putStrLn (show minInt ++ " is the smallest")
