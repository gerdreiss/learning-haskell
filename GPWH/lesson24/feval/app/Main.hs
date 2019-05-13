{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment
import           System.IO

{- lazy file eval
import StringEval

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (countsText . getCounts) input
  putStrLn summary
  hClose file
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
-}

import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO

import           TextEval

{- strict file eval -}
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TIO.readFile fileName
  let summary = countsText . getCounts $ input
  TIO.appendFile "stats.dat" (mconcat [T.pack fileName, " ", summary, "\n"])
  TIO.putStrLn summary
