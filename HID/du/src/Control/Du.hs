{-# LANGUAGE RecordWildCards #-}

module Control.Du where

import           Control.Monad.RWS
import           Data.Du
import           Data.Foldable
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Posix.Types

runApp :: DuM s a -> DuConfig -> s -> IO (a, DuLog s)
runApp app config init = evalRWST app config (DuState 0 (basePath config) init)

traverseDirectoryWith :: DuM s () -> DuM s ()
traverseDirectoryWith app = do
  path <- gets curPath
  content <- liftIO $ listDirectory path
  traverse_ (go path) content
  where
    go path name = modify (newPath $ path </> name) >> app >> modify (restorePath path)
    newPath path st@DuState {..} = st {curDepth = curDepth + 1, curPath = path}
    restorePath path st@DuState {..} = st {curDepth = curDepth - 1, curPath = path}

recordEntry :: FilePath -> FileStatus -> DUApp ()
recordEntry fp fs = do
  ext <- asks ext
  when (needRec fp ext $ isRegularFile fs) (addToTS $ fileSize fs)
  where
    addToTS :: FileOffset -> DUApp ()
    addToTS ofs = modify (\st -> st {st_field = st_field st + ofs})
    needRec _ Nothing _          = True
    needRec fp (Just ext) isFile = isFile && ext `isExtensionOf` fp

logDiffTS :: FileOffset -> DUApp ()
logDiffTS ts = do
  DuState {..} <- get
  tell [(curPath, st_field - ts)]

diskUsage :: DUApp ()
diskUsage = do
  maxDepth <- asks maxDepth
  DuState {..} <- get
  fs <- liftIO $ getFileStatus curPath
  let isDir = isDirectory fs
      shouldLog = isDir && curDepth <= maxDepth
  when isDir $ traverseDirectoryWith diskUsage
  recordEntry curPath fs
  when shouldLog $ logDiffTS st_field
