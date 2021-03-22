module Adapter.Redis.Auth where

import qualified Database.Redis                as R
import qualified Domain.Auth                   as D

import           ClassyPrelude
import           Control.Monad.Catch            ( MonadThrow )
import           Data.Has


type State = R.Connection

type Redis r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

-- | create state from redis url string
-- format: redis::/user:pass@host:port/db
-- sample: redis://abs:def@localhost:6379/0
withState :: String -> (State -> IO a) -> IO a
withState connUrl action = do
  case R.parseConnectInfo connUrl of
    Left  _        -> throwString "Invalid Redis connection URL"
    Right connInfo -> do
      conn <- R.checkedConnect connInfo
      action conn

withConn :: Redis r m => R.Redis a -> m a
withConn action = do
  conn <- asks getter
  liftIO $ R.runRedis conn action

newSession :: Redis r m => D.UserId -> m D.SessionId
newSession userId = do
  sId    <- liftIO $ D.randomVCode 32
  result <- withConn $ R.set (encodeUtf8 sId) (fromString . show $ userId)
  case result of
    Right R.Ok -> return sId
    err        -> throwString $ "Unexpected redis error: " <> show err

findUserIdBySessionId :: Redis r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  result <- withConn $ R.get (encodeUtf8 sId)
  case result of
    Right (Just uIdStr) -> return . readMay . unpack . decodeUtf8 $ uIdStr
    err                 -> throwString $ "Unexpected redis error: " <> show err
