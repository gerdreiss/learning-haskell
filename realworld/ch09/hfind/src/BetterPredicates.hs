module BetterPredicates
  ( betterFind
  ) where

import Control.Exception (IOException (..), bracket, handle)
import Control.Monad     (filterM)
import System.Directory  (Permissions (..), getModificationTime, getPermissions)
import System.FilePath   (takeExtension)
import System.IO         (IOMode (..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath      -- path to directory entry
              -> Permissions   -- permissions
              -> Maybe Integer -- file size (Nothing if not file)
              -> Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where
    check name = do
      perms <- getPermissions name
      size <- getFileSize name
      return (p name perms size)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path =
  handleIO (\_ -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> Just <$> hFileSize h

simpleFileSize :: FilePath -> IO (Maybe Integer)
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path =
  handleIO (\_ -> return Nothing) $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = handle
