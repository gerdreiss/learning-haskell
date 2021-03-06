module Main where

--import           Data.Char
--import           Data.List
--import qualified Data.Text    as T
--import qualified Data.Text.IO as TIO

--main = do
--  [fname] <- getArgs
--  text <- TIO.readFile fname
--  let ws = map head . group . sort . map T.toCaseFold . filter (not . T.null) . map (T.dropAround $ not . isLetter) . T.words $ text
--  TIO.putStrLn $ T.unwords ws
--  print $ length ws

--main = do
--  args <- getArgs
--  case args of
--    [fname] -> processTextFile fname
--    _       -> putStrLn "Usage: vocab-builder filename"

import           Shakespeare        (processTextFile)
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname, n] -> processTextFile fname (read n)
    _ -> putStrLn "Usage: vocab-builder filename number_of_frequent_words"
