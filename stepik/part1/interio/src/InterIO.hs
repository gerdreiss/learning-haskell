module InterIO
  ( main'
  ) where

import           Data.List        (isInfixOf)
import           System.Directory (getDirectoryContents, removeFile)

main' :: IO ()
main' = do
  putStr "Substring: "
  sub <- getLine
  if null sub
    then putStrLn "Cancelled"
    else do
      files <- getDirectoryContents "."
      let deletables = filter (sub `isInfixOf`) files
      sequence_ $ fmap delete deletables
  where
    delete deletable = do
      putStrLn $ "Removing file: " ++ deletable
      removeFile $ deletable


-- nicer, not mine
main'' :: IO ()
main'' = do
    putStr "Substring: "
    substr <- getLine
    if null substr then putStrLn "Cancelled"
    else do
        filtered <- getDirectoryContents "." >>= return . filter (isInfixOf substr)
        mapM_ putStrLn . map (\x -> "Removing file: " ++ x) $ filtered
        mapM_ removeFile filtered

-- also nice, also not mine
-- check <$> that is used instead of >>= return .
deleteMessage = ("Removing file: "++)
deleteFun path = putStrLn (deleteMessage path) >> removeFile path

main''' :: IO ()
main''' = do
    putStr "Substring: "
    subs <- getLine
    case subs of
        [] -> putStrLn "Cancelled"
        _ -> do
            files <- filter (isInfixOf subs) <$> getDirectoryContents "."
            mapM_ deleteFun files

