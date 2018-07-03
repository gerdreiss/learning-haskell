module Main where

import           Lexer

main :: IO ()
main = print $ tokenize "( 1 + 2 )"
