import           System.IO

main :: IO ()
main = do
    handle <- openFile "words.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

