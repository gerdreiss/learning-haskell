module Main where

import           Data.Aeson
import           Data.ByteString.Lazy as B
import           NOAA

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
  let noaaResults = results <$> noaaResponse
  case noaaResults of
    Left err -> print err
    Right noaa -> printResults noaa
