module Main where

import Glitcher
import System.Environment

main :: IO ()
main =  do
    args <- getArgs
    let fileName = head args
    glitch fileName
