module Main where

import           Json0

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile path parser = do
  input <- readFile path
  return $ snd <$> runParser parser input

main :: IO ()
main = undefined
