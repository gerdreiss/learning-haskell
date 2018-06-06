module FindDigit where

import           Data.Char (isDigit)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (c : cs) | isDigit c = Just c
                   | otherwise = findDigit cs

findDigitOrX :: [Char] -> Char
findDigitOrX cs =
    case findDigit cs of
        Just c -> c
        _      -> 'X'
