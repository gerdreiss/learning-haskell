module Domain.Auth where

import           ClassyPrelude
import           Control.Monad.Except
import           Domain.Validation
import           Text.Regex.PCRE.Heavy

--
--
-- Types
--
type VerificationCode = Text

newtype Email =
  Email
    { emailRaw :: Text
    }
  deriving (Show, Eq, Ord)

newtype Password =
  Password
    { passwordRaw :: Text
    }
  deriving (Show, Eq)

data Auth =
  Auth
    { authEmail    :: Email
    , authPassword :: Password
    }
  deriving (Show, Eq)

data RegistrationError =
  RegistrationErrorEmailTaken
  deriving (Show, Eq)

data EmailValidationError =
  EmailValidationErrorInvalidEmail

data PasswordValidationError
  = PasswordValidationErrorLength Int
  | PasswordValidationErrorMustContainUpperCase
  | PasswordValidationErrorMustContainLowerCase
  | PasswordValidationErrorMustContainNumber

--
--
-- Type classes
--
class Monad m =>
      AuthRepo m
  where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailValidationError ())

class Monad m =>
      EmailVerificationNotif m
  where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

-- mock instance implementations
instance AuthRepo IO where
  addAuth (Auth email pass) = do
    putStrLn $ "addding auth: " <> rawEmail email
    return $ Right "fake verification code"
  setEmailAsVerified vcode = do
    putStrLn $ "verifying email with code " <> vcode
    return $ Right ()

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
    [ lengthBetween 5 50 "Should be between 5 and 50"
    , regexMatches [re|\d|] "Should contain a number"
    , regexMatches [re|[A-Z]|] "Should contain an uppercase letter"
    , regexMatches [re|[a-z]|] "Should contain a lowercase letter"
    ]

register ::
     (AuthRepo m, EmailVerificationNotif m)
  => Auth
  -> m (Either RegistrationError ())
register auth =
  runExceptT $ do
    vCode <- ExceptT $ addAuth auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode

verifyEmail :: AuthRepo m => VerificationCode -> m (Either EmailValidationError ())
verifyEmail = setEmailAsVerified
