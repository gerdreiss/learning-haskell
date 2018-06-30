module InterIO
  ( main'
  ) where

import Data.List (isInfixOf)
import System.Directory (getDirectoryContents, removeFile)

main' :: IO ()
main' = do
  putStr "Substring: "
  sub <- getLine
  if null sub
    then putStrLn "Canceled"
    else do
      files <- getDirectoryContents "./test"
      let deletables = filter (sub `isInfixOf`) files
      sequence_ $ fmap delete deletables
  where
    delete deletable = do
      putStrLn $ "Removing file: " ++ deletable
      removeFile $ "./test/" ++ deletable
