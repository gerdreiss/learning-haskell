module Shakespeare (processTextFile) where

import           Data.Char
import           Data.List
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

type Entry = (T.Text, Int) -- one entry

type Vocabulary = [Entry] -- a list of entries

extractVocab :: T.Text -> Vocabulary
extractVocab text = map buildEntry . group . sort $ ws
  where
    ws = map T.toCaseFold . filter (not . T.null) . map cleanWord . T.words $ text
    buildEntry ws@(w:_) = (w, length ws)
    cleanWord = T.dropAround (not . isLetter)

printAllWords :: Vocabulary -> IO ()
printAllWords vocab = do
  putStrLn "All words: "
  TIO.putStrLn . T.unlines . map fst $ vocab

processTextFile :: FilePath -> IO ()
processTextFile fname = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  printAllWords vocab

printWordsCount :: Vocabulary -> IO ()
printWordsCount = print

printFrequentWords :: Vocabulary -> Int -> IO ()
printFrequentWords vocab frequency = printWordsCount $ filter (\v -> snd v <= frequency) vocab
