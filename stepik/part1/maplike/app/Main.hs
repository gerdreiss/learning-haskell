module Main where

import           MapLike

main :: IO ()
main = print (fromList [("key","value")] :: ListMap String String)
