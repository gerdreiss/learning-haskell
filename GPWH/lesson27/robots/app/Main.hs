module Main where

import qualified Data.Map as Map

import           Robots

printCost :: Maybe Double -> IO ()
printCost (Just cost) = print cost
printCost _           = putStrLn "missing item"

main :: IO ()
main = do
  putStrLn "enter a part number 1"
  partNo1 <- getLine
  putStrLn "enter a part number 2"
  partNo2 <- getLine
  let part1 = Map.lookup (read partNo1) partsDB
  let part2 = Map.lookup (read partNo2) partsDB
  let cheapest = min <$> (cost <$> part1) <*> (cost <$> part2)
  printCost cheapest
