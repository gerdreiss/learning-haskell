{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Control.Monad
import           Data.List


eval :: Double -> Double -> Double
eval x i = (x ** i) / product [1..i]

eX :: Double -> Double
eX x = 1 + x + sum (map (eval x) [2..9])

trunc :: Double -> Double
trunc x = fromIntegral (floor (x * 10000)) / 10000

main :: IO()
main = do
    n <- readLn :: IO Int

    forM_ [1..n] $ \_ -> do
        x <- readLn :: IO Double
        print . trunc . eX $ x
