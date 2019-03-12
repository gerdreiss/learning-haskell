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

type InfoP a = FilePath      -- path to directory entry
            -> Permissions   -- permissions
            -> Maybe Integer -- file size (Nothing if not a file)              
            -> a

pathP :: InfoP FilePath            
pathP path _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) = size
sizeP _ _ Nothing     = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y = f w x y == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y = f w x y `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y = f w x y && g w x y

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y = f w x y `q` g w x y

andP = liftP2 (&&)
orP = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ = k

liftP' q f k w x y = f w x y `q` constP k w x y

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ = f w

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
