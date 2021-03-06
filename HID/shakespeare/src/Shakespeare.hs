{-# LANGUAGE OverloadedStrings #-}

module Shakespeare
  ( processTextFile
  ) where

import           Data.Char
import           Data.List
import           Data.Ord
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           System.Environment

type Entry = (T.Text, Int)

type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry . group . sort $ ws
  where
    ws = map T.toCaseFold . filter (not . T.null) . map cleanWord . T.words $ t
    buildEntry ws@(w:_) = (w, length ws)
    cleanWord = T.dropAround (not . isLetter)

allWordsReport :: Vocabulary -> T.Text
allWordsReport = T.append "\nAll words:\n" . T.unlines . map fst

wordsCount :: Vocabulary -> Int
wordsCount = sum . map snd

wordsCountReport :: Vocabulary -> T.Text
wordsCountReport = T.append "\nTotal number of words: " . T.pack . show . wordsCount

wordsByFrequency :: Vocabulary -> Vocabulary
--wordsByFrequency = sortBy (comparing $ Down . snd)
wordsByFrequency = sortOn (Down . snd)

frequentWordsReport :: Vocabulary -> Int -> T.Text
frequentWordsReport vocab n = T.append "\nFrequent words:\n" . T.unlines . map showEntry . take n . wordsByFrequency $ vocab
  where
    showEntry (t, n) = T.append t $ T.pack $ " - " ++ show n

processTextFile :: FilePath -> Int -> IO ()
processTextFile fname n = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  TIO.putStrLn $ allWordsReport vocab
  TIO.putStrLn $ wordsCountReport vocab
  TIO.putStrLn $ frequentWordsReport vocab n
