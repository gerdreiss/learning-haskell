{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}


module Main where

import           Cipher

main :: IO ()
main = do
    putStr "Enter word to encipher: "
    word <- getLine
    putStr "Enter shift: "
    shift <- getLine
    let enciphered = cipher (read shift :: Int) word
    putStr "Enciphered word: "
    putStrLn enciphered
