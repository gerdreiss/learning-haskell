import           System.IO

main = withFile "words.txt" ReadMode (\handle -> do
                                        contents <- hGetContents handle
                                        putStr contents)

