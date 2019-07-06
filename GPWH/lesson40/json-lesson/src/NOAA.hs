{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module NOAA where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy       as B
import           Data.ByteString.Lazy.Char8 as BC
import           Data.Text                  as T
import           GHC.Generics

data NOAAResult = NOAAResult
    { uid          :: T.Text
    , mindate      :: T.Text
    , maxdate      :: T.Text
    , name         :: T.Text
    , datacoverage :: Float
    , resultId     :: T.Text
    } deriving (Show)

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult <$> v .: "uid"
               <*> v .: "mindate"
               <*> v .: "maxdate"
               <*> v .: "name"
               <*> v .: "datacoverage"
               <*> v .: "id"

data Resultset = Resultset
     { offset :: Int
     , count  :: Int
     , limit  :: Int
     } deriving (Show, Generic)

instance FromJSON Resultset

data Metadata = Metadata
    { resultset :: Resultset
    } deriving (Show, Generic)

instance FromJSON Metadata

data NOAAResponse = NOAAResponse
    { metadata :: Metadata
    , results  :: [NOAAResult]
    } deriving (Show, Generic)

instance FromJSON NOAAResponse

printResults :: [NOAAResult] -> IO ()
printResults [] = print "empty result"
printResults results = forM_ results (print . name)

