{-# LANGUAGE OverloadedStrings #-}

module Books where

import qualified Data.Text as T

type Author = T.Text

type Title = T.Text

type Html = T.Text

data Book = Book
  { author :: Author
  , title  :: Title
  } deriving (Show)

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where
    titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
    authorInTags = mconcat ["<em>", author book, "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat ["<html>\n", "<head>\n", "<title>books</title>\n", "<meta charset='utf-8'/>\n", "</head>\n", "<body>\n", booksHtml, "</body>\n", "</html>"]
  where
    booksHtml = mconcat . map bookToHtml $ books
