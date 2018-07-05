module Acronym (abbreviate) where

import           Data.Char

abbreviate :: String -> String
abbreviate = map (toUpper . head) . words . map charOrBlank . insertBlankIntoCamelCase

charOrBlank :: Char -> Char
charOrBlank c
  | toUpper c >= 'A' && toUpper c <= 'Z' = c
  | otherwise = ' '

insertBlankIntoCamelCase :: [Char] -> [Char]
insertBlankIntoCamelCase [] = []
insertBlankIntoCamelCase [a] = [a]
insertBlankIntoCamelCase (c1:c2:cs)
  | isLower c1 && isUpper c2 = c1:' ':c2:insertBlankIntoCamelCase cs
  | otherwise = c1:c2:insertBlankIntoCamelCase cs
