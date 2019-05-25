{-# LANGUAGE OverloadedStrings #-}

module Marc2Html where

import qualified Data.ByteString    as B
import           Data.Maybe
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E


type Author = T.Text
type Title  = T.Text
type Html   = T.Text

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

data Book = Book
  { author :: Author
  , title  :: Title
  } deriving (Show)

book1 :: Book
book1 =
  Book
    { title = "The Conspiracy Against the Human Race"
    , author = "Ligotti, Thomas"
    }

book2 :: Book
book2 =
 Book
  { title = "A Short History of Decay"
  , author = "Cioran, Emil"
  }

book3 :: Book
book3 =
 Book
  { title = "The Tears of Eros"
  , author = "Bataille, Georges"
  }

books :: [Book]
books = [book1, book2, book3]

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where
    titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
    authorInTags = mconcat ["<em>", author book, "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat [ "<html>\n"
                            , "<head>\n"
                            , "<title>books</title>\n"
                            , "<meta charset='utf-8'/>\n"
                            , "</head>\n"
                            , "<body>\n"
                            , booksHtml
                            , "</body>\n"
                            , "</html>"
                            ]
  where
    booksHtml = mconcat . map bookToHtml $ books

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty
                        then []
                        else next : allRecords rest
  where (next, rest) = nextAndRest marcStream


nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream =  B.splitAt recordLength marcStream
  where recordLength = getRecordLength marcStream

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt $ B.take 5 leader

leaderLength :: Int
leaderLength = 24

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength
