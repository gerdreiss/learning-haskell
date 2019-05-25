module Main where

import Marc2Html

import qualified Data.ByteString    as B

main :: IO ()
main = do
  marcData <- B.readFile "ohsu_ncnm_wscc_bibs.mrc"
  let marcRecords = allRecords marcData
  print (length marcRecords)
