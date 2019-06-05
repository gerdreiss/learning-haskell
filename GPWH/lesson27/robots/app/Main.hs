module Main where

import Robots

main :: IO ()
main = putStrLn . mconcat $ allPartsHtml
