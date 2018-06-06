module Main where

import           Lib

main :: IO ()
main = do
    putStrLn $ show $ sum'n'count 01123
    putStrLn $ getSecondFrom "a" 9 False
