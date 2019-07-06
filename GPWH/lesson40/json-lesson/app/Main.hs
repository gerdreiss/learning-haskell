module Main where

import           Data.Aeson
import           Data.ByteString.Lazy as B
import           NOAA

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults
