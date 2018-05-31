module Lib
    ( formatGrid
    , outputGrid
    , skew
    , findWordInCellLinePrefix
    , findWordInLine
    , findWord
    , findWords
    , zipOverGrid
    , zipOverGridWith
    , cell2char
    , gridWithCoords
    , Cell(Cell,Indent)
    ) where

import           Data.List  (isInfixOf, transpose)
import           Data.Maybe (catMaybes, listToMaybe)


data Cell = Cell (Int, Int) Char
          | Indent
           deriving (Eq, Ord, Show)

type Grid a = [[a]]

zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map


coordsGrid :: Grid (Int, Int)
coordsGrid =
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols


gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid


outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn $ formatGrid grid


formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char


cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent     = '?'


findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
    let found = findWordInCellLinePrefix [] word line
    in case found of
        Nothing     -> findWordInLine word (tail line)
        cs@(Just _) -> cs


skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
   where indent line = Indent : line


diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skew


getLines :: Grid Cell -> [[Cell]]
getLines grid =
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize $ map reverse grid
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)


findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
    let lines = getLines grid
        foundWords = map (findWordInLine word) lines
    in listToMaybe (catMaybes foundWords)


findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords


findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (chr : chrs) (cell : cells) | chr == cell2char cell =
    findWordInCellLinePrefix (cell : acc) chrs cells
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing

