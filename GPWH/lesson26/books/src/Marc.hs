module Marc where

import           Books
import           Fields
import           Records

import qualified Data.ByteString as B
import           Data.Maybe

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map createBooks justPairs
  where
    createBooks (title, author) = Book {title = fromJust title, author = fromJust author}
    justPairs = filter isJustPair pairs
    isJustPair (title, author) = isJust title && isJust author
