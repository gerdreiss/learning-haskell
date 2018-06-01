module Main where

import           System.IO
import           Data
import           Lib


main :: IO ()
main = do
    let game = makeGame grid languages
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


