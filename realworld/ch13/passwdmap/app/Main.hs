module Main where

import           Control.Monad      (when)
import           PasswdMap
import           System.Environment (getArgs)
import           System.Exit

main :: IO ()
main = do
    -- Load the command-line arguments
    args <- getArgs

    -- If we don't have the right number of args,
    -- give an error and abort

    when (length args /= 1) $ do
        putStrLn "Syntax: passwdmap filename"
        exitFailure

    -- Read the file lazily
    content <- readFile (head args)
    let maps = inputToMaps content
    mainMenu maps
