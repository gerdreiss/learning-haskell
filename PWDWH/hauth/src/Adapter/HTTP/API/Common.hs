module Adapter.HTTP.API.Common where

import qualified Text.Digestive.Aeson          as DF
import qualified Text.Digestive.Form           as DF

import           Adapter.HTTP.Common
import           ClassyPrelude
import           Data.Aeson              hiding ( json )
import           Domain.Auth
import           Network.HTTP.Types.Status
import           Web.Scotty.Trans

-- * Forms

parseAndValidateJSON
  :: (ScottyError e, MonadIO m, ToJSON v) => DF.Form v m a -> ActionT e m a
parseAndValidateJSON form = do
  val              <- jsonData `rescue` (\_ -> return Null)
  validationResult <- lift $ DF.digestJSON form val
  case validationResult of
    (view, Nothing) -> do
      status status400
      json $ DF.jsonErrors view
      finish
    (_, Just result) -> return result

reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = do
  maybeUserId <- getCurrentUserId
  case maybeUserId of
    Nothing -> do
      status status401
      json ("AuthRequired" :: Text)
      finish
    Just userId -> return userId

-- * Error response
errorResponse :: (ToJSON a) => a -> Value
errorResponse val = object ["error" .= val]
