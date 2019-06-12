module Main where

import           Gaming
import           System.IO (hFlush, stdout)

main :: IO ()
main = putStrLn "What is your name?" >>
       hFlush stdout >>
       getLine >>=
       nameStatement >>=
       putStrLn >>
       putStrLn "Welcome!"

nameStatement :: String -> IO String
nameStatement name = return ("Hello, " ++ name ++ "!")
