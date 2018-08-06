{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

main :: IO()
main = do
    n <- readLn :: IO Int
    mapM_ (\_ -> putStrLn "Hello World") [1..n]
