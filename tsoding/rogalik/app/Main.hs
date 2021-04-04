module Main where

import           Control.Display
import           Control.Monad
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
gameLoop rogalik display =
  unless (rogalikQuit rogalik) $ do
    let display' = renderRogalik rogalik display
    putStrLn $ renderDisplay display'
    rogalik' <- userInput rogalik
    gameLoop rogalik' display'

userInput :: Rogalik -> IO Rogalik
userInput rogalik = putStr "> " >> getLine >>= processUserInput rogalik

processUserInput :: Rogalik -> String -> IO Rogalik
processUserInput r "q" = return $ r {rogalikQuit = True}
processUserInput r "w" = return $ rogalikMove U r
processUserInput r "s" = return $ rogalikMove D r
processUserInput r "a" = return $ rogalikMove L r
processUserInput r "d" = return $ rogalikMove R r
processUserInput r "help" =
  putStrLn "Use gamer's keybindings (WASD) to navigate" >> return r
processUserInput r input = putStrLn ("Unknown command: " <> input) >> return r
