module Main where

import           Control.Display
import           Control.Rogalik
import           Data.Display
import           Data.Geom
import           Data.Rogalik
import           System.Exit
import           Text.Printf

main :: IO ()
main = do
  let display = mkDisplay (Size 80 30) ' '
      rogalik = generateRogalik
   in gameLoop rogalik display

gameLoop :: Rogalik -> Display -> IO ()
gameLoop rogalik display = do
  let display' = renderRogalik rogalik display
  putStrLn $ renderDisplay display'
  rogalik' <- userInput rogalik
  gameLoop rogalik' display'

userInput :: Rogalik -> IO Rogalik
userInput rogalik = putStr "> " >> getLine >>= processUserInput rogalik

processUserInput :: Rogalik -> String -> IO Rogalik
processUserInput _ "q" = exitSuccess :: IO Rogalik
processUserInput r "w" = return $ rogalikMove U r
processUserInput r "s" = return $ rogalikMove D r
processUserInput r "a" = return $ rogalikMove L r
processUserInput r "d" = return $ rogalikMove R r
processUserInput r "help" =
  putStrLn "Use gamer's keybindings (WASD) to navigate" >> return r
processUserInput r input = putStrLn ("Unknown command: " <> input) >> return r
