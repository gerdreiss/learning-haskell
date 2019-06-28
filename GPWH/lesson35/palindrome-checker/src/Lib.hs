{-# LANGUAGE OverloadedStrings  #-}
module Lib
    ( isPalindrome
    ) where

import qualified Data.Text as T
import Data.Char (toLower, isSpace, isPunctuation)


stripWhitespace :: T.Text -> T.Text
stripWhitespace text = T.filter (not . isSpace) text

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.filter (not . isPunctuation) text

preProcess :: T.Text -> T.Text
preProcess = stripWhitespace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where cleanText = preProcess text

