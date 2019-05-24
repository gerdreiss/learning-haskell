module Main where

import Marc2Html

import qualified Data.Text.IO       as TIO

main :: IO ()
main = TIO.writeFile "books.html" (booksToHtml books)

