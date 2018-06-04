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
    , Game(gameGrid, gameWords)
    , makeGame
    , totalWords
    , score
    , playGame
    , formatGame
    , completed
    , makeRandomGrid
    , fillInBlanks
    ) where

-- imports
import           System.Random
import           Data.List  (isInfixOf, transpose)
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Char (toLower)
import qualified Data.Map as M

-- types
type Grid a = [[a]]

-- data structs
data Cell = Cell (Int, Int) Char | Indent deriving (Eq, Ord, Show)
data Game = Game {
                gameGrid :: Grid Cell,
                gameWords :: M.Map String (Maybe [Cell])
            }
            deriving (Show)


-- create game
makeGame :: Grid Char -> [String] -> Game
makeGame grid words =   
    let gwc = gridWithCoords grid
        tuplify word = (word, Nothing)
        list = map tuplify words
        dict = M.fromList list
    in Game gwc dict


gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid


zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith


coordsGrid :: Grid (Int, Int)
coordsGrid =
    let rows = map repeat [0..]
        cols = repeat [0..]
    in zipOverGrid rows cols


zipOverGrid :: Grid a -> Grid b -> Grid (a, b)
zipOverGrid = zipWith zip


makeRandomGrid gen =
    let (gen1, gen2) = split gen
        row = randomRs ('A', 'Z') gen1
    in row : makeRandomGrid gen2


fillInBlanks gen grid =
    let r = makeRandomGrid gen
        fill '_' r = r
        fill c _   = c
    in zipOverGridWith fill grid r


-- play game
playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word =
    let foundWord = findWord (gameGrid game) word
    in case foundWord of
        Nothing -> game
        Just _ ->
            let oldDict = gameWords game
                newDict = M.insert word foundWord oldDict
            in game { gameWords = newDict } 


-- game helper functions
totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game


score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game


completed :: Game -> Bool
completed game = score game == totalWords game


-- lookup functions
findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words =
    let foundWords = map (findWord grid) words
    in catMaybes foundWords


findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
    let lines = getLines grid
        foundWords = map (findWordInLine word) lines
    in listToMaybe (catMaybes foundWords)


findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
    let found = findWordInCellLinePrefix [] word line
    in case found of
        Nothing     -> findWordInLine word (tail line)
        cs@(Just _) -> cs


findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (chr : chrs) (cell : cells) | chr == cell2char cell =
    findWordInCellLinePrefix (cell : acc) chrs cells
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing
        

-- helper functions
getLines :: Grid Cell -> [[Cell]]
getLines grid =
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize $ map reverse grid
        lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in lines ++ (map reverse lines)


diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skew


skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
    where indent line = Indent : line


-- format functions
formatGame :: Game -> String
formatGame game = formatGameGrid game
                ++ "\n\n"
                ++ (show $ score game)
                ++ "/"
                ++ (show $ totalWords game)


formatGameGrid :: Game -> String
formatGameGrid game =
    let grid = gameGrid game
        dict = gameWords game :: M.Map String (Maybe [Cell])
        cellSet = concat . catMaybes . M.elems $ dict
        formatCell cell =
            let char = cell2char cell
            in if cell `elem` cellSet then char else toLower char
        charGrid = mapOverGrid formatCell grid
    in unlines charGrid


formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char


mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map


outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn $ formatGrid grid


cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent     = '?'
