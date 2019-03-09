module RecursiveContents
  ( getRecursiveContents
  ) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))


getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents root = do
  names <- getDirectoryContents root
  let properNames = filter (`notElem` [".", ".."]) names
  paths <-
    forM properNames $ \name -> do
      let path = root </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then getRecursiveContents path
        else return [path]
  return (concat paths)
