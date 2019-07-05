{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.ByteString.Lazy       as B
import           Data.ByteString.Lazy.Char8 as BC
import           Data.Text                  as T
import           GHC.Generics

data Book = Book
    { title  :: T.Text
    , author :: T.Text
    , year   :: Int
    } deriving (Show, Generic)

data Name = Name
    { firstName :: T.Text
    , lastName  :: T.Text
    } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book
instance FromJSON Name
instance ToJSON Name

myBook :: Book
myBook = Book
 { author = "Will Kurt"
 , title = "Learn Haskell"
 , year = 2017
 }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

main :: IO ()
main = print bookFromJSON
