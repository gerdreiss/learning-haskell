module Main where

import Tools


performCommand :: String -> IO ()
performCommand command
   | command == "users" = printUsers >> main
   | command == "tools" = printTools >> main
   | command == "adduser" = promptAndAddUser >> main
   | command == "checkout" = promptAndCheckout >> main
   | command == "checkin" = promptAndCheckin >> main
   | command == "in" = printAvailable >> main
   | command == "out" = printCheckedOut >> main
   | command == "quit" = putStrLn "bye!"
   | otherwise = putStrLn "Sorry command not found" >> main


main :: IO ()
main = do
   putStrLn "Enter a command"
   command <- getLine
   performCommand command
