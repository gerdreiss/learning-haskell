module DNA (toRNA) where

import           Data.Maybe

toRNA :: String -> Maybe String
toRNA = traverse (`lookup` [('C', 'G'), ('G', 'C'), ('T', 'A'), ('A', 'U')])
--      traverse r2d
--      allOrNothing . map r2d

-- Unnecessary function because of traverse
allOrNothing :: [Maybe Char] -> Maybe String
allOrNothing xs
    | Nothing `elem` xs = Nothing
    | otherwise         = Just $ catMaybes xs

-- Unnecessary function because of lookup
r2d :: Char -> Maybe Char
r2d d =
    -- Using case of
    case d of
        'G' -> Just 'C'
        'C' -> Just 'G'
        'T' -> Just 'A'
        'A' -> Just 'U'
        _   -> Nothing
    -- Using guards
    {-
    | d == 'G'  = Just 'C'
    | d == 'C'  = Just 'G'
    | d == 'T'  = Just 'A'
    | d == 'A'  = Just 'U'
    | otherwise = Nothing
    -}
