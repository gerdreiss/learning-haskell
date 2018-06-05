{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where

import           Data
import           ShowParser (parseShow)


rec1 = MkPersonRecord
    "Wim Vanderbauwhede"
    (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
    557188
    [Green, Red]

rec2 = MkPersonRecord
    "Jeremy Singer"
    (MkAddress "School of Computing Science" 17 "Lilybank Gdns" "Glasgow" "G12 8QQ")
    42
    [Blue, Yellow]

rec_str = show [rec1, rec2]

main :: IO ()
main = putStrLn $ parseShow rec_str
