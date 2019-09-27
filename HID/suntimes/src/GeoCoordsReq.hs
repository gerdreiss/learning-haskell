{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GeoCoordsReq where

import           Data.Aeson
import           Data.Text
import           Data.Text.Encoding
import           GHC.Generics
import           Network.HTTP.Req
import           Control.Monad.Reader           ( ReaderT
                                                , MonadIO
                                                , MonadReader
                                                , runReaderT
                                                )

type Address = Text

data GeoCoords = GeoCoords
  { lat :: Text
  , lon :: Text
  } deriving (Show, Generic)

instance FromJSON GeoCoords

data WebAPIAuth = WebAPIAuth
  { timeZoneDBkey :: Text
  , email :: Text
  , agent :: Text
  } deriving (Generic, Show)

instance FromJSON WebAPIAuth

newtype MyApp a = MyApp {
      runApp :: ReaderT WebAPIAuth IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader WebAPIAuth)

runMyApp :: MyApp a -> WebAPIAuth -> IO a
runMyApp app = runReaderT (runApp app)

getCoords :: Address -> WebAPIAuth -> IO GeoCoords
getCoords addr wauth = do
  let url       = https "nominatim.openstreetmap.org" /: "search"
      reqParams = mconcat
        [ "q" =: addr
        , "format" =: ("json" :: Text)
        , "limit" =: (1 :: Int)
        , "email" =: email wauth
        , header "User-Agent" (encodeUtf8 $ agent wauth)
        ]
      request = req GET url NoReqBody jsonResponse reqParams
  res <- responseBody <$> runReq defaultHttpConfig request
  case res of
    []           -> pure GeoCoords { lat = "", lon = "" } -- TODO: this is a no answer situation
    (coords : _) -> pure coords
