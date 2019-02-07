{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (catch, finally, IOException)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO

main :: IO ()
main = withTempFile "temp.txt" action

action :: FilePath -> Handle -> IO ()
action tempname temph = do
  putStrLn "Welcome to tempfile.hs"
  putStrLn $ "I have a temporary file at " ++ tempname
  pos <- hTell temph
  putStrLn $ "My initial position is " ++ show pos
  let tempdata = show [1 .. 10]
  putStrLn $
    "Writing one line containing " ++
    show (length tempdata) ++ " bytes: " ++ tempdata
  hPutStrLn temph tempdata
  pos <- hTell temph
  putStrLn $ "After wrinting, my new position is " ++ show pos
  putStr "The file content is: "
  hSeek temph AbsoluteSeek 0
  c <- hGetContents temph
  putStrLn c
  putStrLn "Which could be expressed as this Haskell literal: "
  print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile p f = do
  tempdir <- catch getTemporaryDirectory (\(e :: IOException) -> return ".")
  (tempfile, temph) <- openTempFile tempdir p
  finally
    (f tempfile temph)
    (do hClose temph
        removeFile tempfile)
