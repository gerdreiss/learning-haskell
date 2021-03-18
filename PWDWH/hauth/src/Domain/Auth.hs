{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Domain.Auth
  ( -- * Types
    Auth (..),
    Email,
    mkEmail,
    rawEmail,
    Password,
    mkPassword,
    rawPassword,
    UserId,
    VerificationCode,
    SessionId,
    RegistrationError (..),
    EmailVerificationError (..),
    LoginError (..),

    -- * Ports
    AuthRepo (..),
    EmailVerificationNotif (..),
    SessionRepo (..),

    -- * Use cases
    register,
    verifyEmail,
    login,
    resolveSessionId,
    getUser,
  )
where

import           ClassyPrelude
import           Control.Monad.Except
import           Domain.Validation
import           Katip
import           Text.Regex.PCRE.Heavy

--
--
-- Types
--
type VerificationCode = Text
type UserId = Int
type SessionId = Text

newtype Email = Email
  { emailRaw :: Text
  }
  deriving (Show, Eq, Ord)

newtype Password = Password
  { passwordRaw :: Text
  }
  deriving (Show, Eq)

data Auth = Auth
  { authEmail    :: Email,
    authPassword :: Password
  }
  deriving (Show, Eq)

data RegistrationError
  = RegistrationErrorEmailTaken
  deriving (Show, Eq)

data EmailVerificationError
  = EmailVerificationErrorInvalidEmail
  | EmailVerificationErrorInvalidCode

data PasswordValidationError
  = PasswordValidationErrorLength Int
  | PasswordValidationErrorMustContainUpperCase
  | PasswordValidationErrorMustContainLowerCase
  | PasswordValidationErrorMustContainNumber

data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Show, Eq)

--
--
-- Type classes
--
class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError (UserId, Email))
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)
  getUser :: AuthRepo m => UserId -> m (Maybe Email)
  getUser = findEmailFromUserId

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)
  resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
  resolveSessionId = findUserIdBySessionId

-- mock instance implementations
instance AuthRepo IO where
  addAuth (Auth email _) = do
    putStrLn $ "addding auth: " <> rawEmail email
    return $ Right (1, "fake verification code")
  setEmailAsVerified vcode = do
    putStrLn $ "verifying email with code " <> vcode
    return $ Left EmailVerificationErrorInvalidEmail
  findUserByAuth _ = return $ Just (1, True)
  findEmailFromUserId _ =
    return $ either (const Nothing) Just (mkEmail "test@example.com")

instance EmailVerificationNotif IO where
  notifyEmailVerification email vcode =
    putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode

--
--
-- Functions
--
rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail =
  validate
    Email
    [ regexMatches
        [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
        "Not a valid email"
    ]

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword =
  validate
    Password
    [ lengthBetween 5 50 "Should be between 5 and 50",
      regexMatches [re|\d|] "Should contain a number",
      regexMatches [re|[A-Z]|] "Should contain an uppercase letter",
      regexMatches [re|[a-z]|] "Should contain a lowercase letter"
    ]

withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

register :: (KatipContext m, AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth =
  runExceptT $ do
    (uId, vCode) <- ExceptT $ addAuth auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode
    withUserIdContext uId $
      $(logTM) InfoS $ ls (rawEmail email) <> " is registered successfully"

verifyEmail :: (KatipContext m, AuthRepo m) => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail vCode = runExceptT $ do
  (uId, email) <- ExceptT $ setEmailAsVerified vCode
  withUserIdContext uId $
    $(logTM) InfoS $ ls (rawEmail email) <> " is verified successfully"
  return ()

login :: (KatipContext m, AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _) -> withUserIdContext uId . lift $ do
      sId <- newSession uId
      $(logTM) InfoS $ ls (rawEmail $ authEmail auth) <> " logged in successfully"
      return sId

--resolveSessionId :: (SessionRepo m) => SessionId -> m (Maybe UserId)
--resolveSessionId = findUserIdBySessionId

--getUser :: (AuthRepo m) => UserId -> m (Maybe Email)
--getUser = findEmailFromUserId
