module Pangram (isPangram) where

import           Data.Char
import           Data.List

-- my version
isPangram :: String -> Bool
isPangram text = all (`elem` (map toLower text)) ['a'..'z']

-- this version is nice, too:
isPangramF :: String -> Bool
isPangramF xs = "abcdefghijklmnopqrstuvwxyz" == f xs
  where f = nub . sort . filter (isLetter) . map (toLower)
