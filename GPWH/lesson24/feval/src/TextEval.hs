{-# LANGUAGE OverloadedStrings #-}

module TextEval
  ( getCounts
  , countsText
  ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TI

getCounts :: T.Text -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = T.length input
    wordCount = length . T.words $ input
    lineCount = length . T.lines $ input

countsText :: (Int, Int, Int) -> T.Text
countsText (cc, wc, lc) =
  T.pack (unwords ["chars: ", show cc, " words: ", show wc, " lines: ", show lc])
