module Lib
    ( grid
    , languages
    , formatGrid
    , outputGrid
    , findWordInLine
    , findWord
    . findWords
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]


grid = [ "__C________R___"
       , "__SI________U__"
       , "__HASKELL____B_"
       , "__A__A_____S__Y"
       , "__R___B___C____"
       , "__PHP____H_____"
       , "____S_LREP_____"
       , "____I_M__Y__L__"
       , "____L_E__T_O___"
       , "_________HB____"
       , "_________O_____"
       , "________CN_____"
       ]

languages = [ "BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"
            ]


outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn $ formatGrid grid


formatGrid :: Grid -> String
formatGrid = unlines


findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf


skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
   where indent line = '_' : line


diagonalize :: Grid -> Grid
diagonalize = transpose . skew


getLines :: Grid -> [String]
getLines grid =
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize $ map reverse grid
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)


findWord :: Grid -> String -> Maybe String
findWord grid word =
    let lines = getLines grid
        found = or $ map (findWordInLine word) lines
    in if found then Just word else Nothing


findWords :: Grid -> [String] -> [String]
findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords
