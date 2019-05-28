module Main where

import           Marc

import qualified Data.ByteString as B
import qualified Data.Text.IO    as TIO

main :: IO ()
main = do
  marcData <- B.readFile "ohsu_ncnm_wscc_bibs.mrc"
  let processed = processRecords 500 marcData
  TIO.writeFile "books.html" processed
