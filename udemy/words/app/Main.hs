module Main where

import           System.IO
import           System.Random
import           Data
import           Lib


main :: IO ()
main = do
    gen <- newStdGen
    let filledInGrid = fillInBlanks gen grid
    let game = makeGame filledInGrid languages
    hSetBuffering stdout NoBuffering
    playTurn game

playTurn :: Game = do
    putStrLn . formatGame $ game
    putStr "Please enter a word: "
    word <- getLine
    let newGame = playGame game word
    if completed newGame then
        putStrLn "Congratulations!"
    else
        playTurn newGame


