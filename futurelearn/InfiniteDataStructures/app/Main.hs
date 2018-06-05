{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where

import           Fibonacci
import           PrimeNumbers

main :: IO ()
main =
  mapM_ print $ take 30 (map show primes)
  -- mapM_ print $ take 30 (map show fibs)
