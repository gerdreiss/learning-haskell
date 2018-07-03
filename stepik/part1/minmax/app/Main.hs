module Main where

import           Data.List hiding (maximum, minimum)
import           MinMax
import           Prelude   hiding (maximum, minimum)

main :: IO ()
main = print $ minimum "abbcbba"
