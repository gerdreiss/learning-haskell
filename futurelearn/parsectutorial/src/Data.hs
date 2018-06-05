{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data
    ( PersonRecord (MkPersonRecord)
    , Address (MkAddress)
    , Label (Green, Red, Blue, Yellow)
    ) where

data PersonRecord  = MkPersonRecord {
    name    :: String,
    address :: Address,
    id      :: Integer,
    labels  :: [Label]
} deriving (Show)

data Address = MkAddress {
    line1    :: String,
    number   :: Integer,
    street   :: String,
    town     :: String,
    postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)
