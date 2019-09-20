{-# LANGUAGE RecordWildCards #-}

module Control.Du where

import           Control.Monad.RWS
import           Data.Du
import           Data.Foldable
import           Options.Applicative
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

mkConfig :: Parser DuConfig
mkConfig = DuConfig
  <$> strArgument (metavar "DIRECTORY" <> value "." <> showDefault)
  <*> option auto (metavar "DEPTH" <> short 'd' <> long "depth" <> value 0 <> showDefault <> help "Maximum depth of reporting ")
  <*> optional (strOption (metavar "EXT" <> long "extension" <> short 'e' <> help "Filter files by extension"))

printLog :: Show s => DuLog s -> IO ()
printLog = traverse_ printEntry
  where
    printEntry (fp, s) = do
      putStr $ show s ++ "\t"
      putStrLn fp

work :: DuConfig -> IO ()
work config = do
  (_, xs) <- runApp fileCount config 0
  putStrLn "File count:"
  printLog xs

fileCount :: DuM Int ()
fileCount = do
    DuState {..} <- get
    fs <- liftIO $ getFileStatus curPath
    when (isDirectory fs) $ do
      DuConfig {..} <- ask
      when (curDepth <= maxDepth) $ traverseDirectoryWith fileCount
      files <- liftIO $ listDirectory curPath
      tell [(curPath, length $ filterFiles ext files)]
  where
    filterFiles Nothing = id
    filterFiles (Just ext) = filter (ext `isExtensionOf`)
