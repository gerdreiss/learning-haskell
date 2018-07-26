module Main where

import           Hamming

main :: IO ()
main = print $ distance "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT"
