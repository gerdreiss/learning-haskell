{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Control.Monad
import           Data.Array
import           Data.Bits
import           Data.List
import           Data.List.Split
import           Data.Set
import           Debug.Trace
import           System.Environment
import           System.IO
import           System.IO.Unsafe

eval :: Double -> Double -> Double
eval x i = (x ** i) / product [1 .. i]

eX :: Double -> Double
eX x = 1 + x + sum (Data.List.map (eval x) [2 .. 9])

trunc :: Double -> Double
trunc x = fromIntegral (floor (x * 10000)) / 10000

main :: IO()
main = do
    n <- readLn :: IO Int

    forM_ [1..n] $ \n_itr -> do
        x <- readLn :: IO Double
        print . trunc . eX $ x
