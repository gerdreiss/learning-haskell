module Adapter.PostgreSQL.Auth where

import           ClassyPrelude
import           Control.Monad.Catch            ( MonadThrow )
import           Data.Has
import           Data.Pool
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration

import qualified Domain.Auth                   as D

type State = Pool Connection

type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

data Config = Config
  { configUrl                  :: ByteString
  , configStripeCount          :: Int
  , configMaxOpenConnPerStripe :: Int
  , configIdleConnTimeout      :: NominalDiffTime
  }

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn

withPool :: Config -> (State -> IO a) -> IO a
withPool cfg = bracket initPool cleanPool
 where
  initPool = createPool openConn
                        closeConn
                        (configStripeCount cfg)
                        (configIdleConnTimeout cfg)
                        (configMaxOpenConnPerStripe cfg)
  cleanPool = destroyAllResources
  openConn  = connectPostgreSQL (configUrl cfg)
  closeConn = close

withState :: Config -> (State -> IO a) -> IO a
withState cfg action = withPool cfg $ \state -> do
  migrate state
  action state

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _                  -> return ()
 where
  cmds =
    [ MigrationInitialization
    , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
    ]

addAuth
  :: PG r m
  => D.Auth
  -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth (D.Auth email pass) = do
  -- generate vCode
  vCode  <- liftIO $ D.emailVerificationCode email
  -- issue query
  result <- withConn $ \conn ->
    try $ query conn qry (D.rawEmail email, D.rawPassword pass, vCode)
  -- interpret result
  case result of
    Right [Only uId] -> return $ Right (uId, vCode)
    Right _ -> throwString "Should not happen: PG doesn't return userId"
    Left err@SqlError { sqlState = state, sqlErrorMsg = msg } ->
      if state == "23505" && "auths_email_key" `isInfixOf` msg
        then return (Left D.RegistrationErrorEmailTaken)
        else throwString ("Unhandled PG exception: " <> show err)
 where
  qry
    = "insert into auths (email, pass, email_verification_code, is_email_verified) \
      \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"


setEmailAsVerified
  :: PG r m
  => D.VerificationCode
  -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = withConn updateAuths >>= processResult
 where
  updateAuths conn = query conn qry (Only vCode)
  qry
    = "update auths set is_email_verified = 't' \
      \where email_verification_code = ? \
      \returning id, cast (email as text)"
  processResult [(uId, mail)] = case D.mkEmail mail of
    Right email -> return $ Right (uId, email)
    _ ->
      throwString
        $  "Should not happen: email in DB is not valid: "
        <> unpack mail
  processResult _ = return $ Left D.EmailVerificationErrorInvalidCode


findUserbyAuth :: PG r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserbyAuth (D.Auth email pass) = withConn queryAuths >>= processResult
 where
  queryAuths conn = query conn qry (D.rawEmail email, D.rawPassword pass)
  qry
    = "select id, is_email_verified \
          \from auths \
          \where email = ? and pass = crypt(?, pass)"
  processResult [(uId, isVerified)] = return $ Just (uId, isVerified)
  processResult _                   = return Nothing

