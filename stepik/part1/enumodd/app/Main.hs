module Main where

import           EnumOdd

main :: IO ()
main = print(toEnum 1 :: Odd)
