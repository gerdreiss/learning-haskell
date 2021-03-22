module Lib
  ( someFunc
  ) where

import qualified Adapter.InMemory.Auth         as M
import qualified Adapter.PostgreSQL.Auth       as PG
import qualified Adapter.Redis.Auth            as Redis
import           ClassyPrelude
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.Except
import           Domain.Auth
import           Katip

--type State = TVar M.State
--type State = (PG.State, TVar M.State)
type State = (PG.State, Redis.State, TVar M.State)

newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadFail,
      MonadReader State,
      MonadIO,
      MonadThrow,
      KatipContext,
      Katip
    )

instance AuthRepo App where
  addAuth             = PG.addAuth
  setEmailAsVerified  = PG.setEmailAsVerified
  findUserByAuth      = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession            = Redis.newSession
  findUserIdBySessionId = Redis.findUserIdBySessionId

someFunc :: IO ()
someFunc = withKatip $ \le -> do
--  state <- newTVarIO M.initialState
--  run le state action
  mState <- newTVarIO M.initialState
  PG.withState pgCfg $ \pgState -> Redis.withState redisCfg
    $ \redisState -> run le (pgState, redisState, mState) action
 where
  pgCfg = PG.Config { PG.configUrl = "postgresql://localhost/hauth"
                    , PG.configStripeCount          = 2
                    , PG.configMaxOpenConnPerStripe = 5
                    , PG.configIdleConnTimeout      = 10
                    }
  redisCfg = "redis://localhost:6379/0"

withKatip :: (LogEnv -> IO a) -> IO a
withKatip = bracket createLogEnv closeScribes
 where
  createLogEnv = do
    logEnv       <- initLogEnv "HAuth" "prod"
    stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
    registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

run :: LogEnv -> State -> App a -> IO a
run le state = runKatipContextT le () mempty . flip runReaderT state . unApp

action :: App ()
action = do
  let email = either undefined id $ mkEmail "ecky@test.com"
      passw = either undefined id $ mkPassword "1234ABCDefgh"
      auth  = Auth email passw
  _                     <- register auth
  Just vCode            <- M.getNotificationsForEmail email
  _                     <- verifyEmail vCode
  Right session         <- login auth
  Just  uId             <- resolveSessionId session
  Just  registeredEmail <- getUser uId
  print (session, uId, registeredEmail)
