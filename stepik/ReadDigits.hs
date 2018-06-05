module ReadDigits where

import           Data.Char

readDigits :: String -> (String, String)
readDigits s = span isDigit s
