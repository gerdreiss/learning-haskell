module Adapter.HTTP.API.Auth where

import qualified Text.Digestive.Form           as DF

import           Adapter.HTTP.API.Common
import           Adapter.HTTP.Common
import           ClassyPrelude
import           Domain.Auth
import           Katip
import           Network.HTTP.Types.Status
import           Text.Digestive.Form            ( (.:) )
import           Web.Scotty.Trans


routes
  :: ( ScottyError e
     , MonadIO m
     , KatipContext m
     , AuthRepo m
     , EmailVerificationNotif m
     , SessionRepo m
     )
  => ScottyT e m ()
routes = do
  -- register
  post "/api/auth/register"
    $   parseAndValidateJSON authForm
    >>= (lift . register)
    >>= either processRegisterError return

  -- verify email
  post "/api/auth/verifyEmail"
    $   parseAndValidateJSON verifyEmailForm
    >>= (lift . verifyEmail)
    >>= either processVerificationError return

  -- login
  post "/api/auth/login"
    $   parseAndValidateJSON authForm
    >>= (lift . login)
    >>= either processLoginError (void . setSessionIdInCookie)

  -- get user
  get "api/users" $ reqCurrentUserId >>= (lift . getUser) >>= maybe
    (raise $ stringError "Should not happen: SessionId map to invalid UserId")
    (json . rawEmail)

 where
  processRegisterError RegistrationErrorEmailTaken =
    status status400 >> json ("EmailTaken" :: Text)
  processVerificationError EmailVerificationErrorInvalidEmail =
    status status400 >> json ("InvalidEmail" :: Text)
  processVerificationError EmailVerificationErrorInvalidCode =
    status status400 >> json ("InvalidCode" :: Text)
  processLoginError LoginErrorInvalidAuth =
    status status400 >> json ("InvalidAuth" :: Text)
  processLoginError LoginErrorEmailNotVerified =
    status status400 >> json ("EmailNotVerified" :: Text)

authForm :: (Monad m) => DF.Form [Text] m Auth
authForm = Auth <$> "email" .: emailForm <*> "password" .: passwordForm
 where
  emailForm    = DF.validate (toResult . mkEmail) (DF.text Nothing)
  passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)

verifyEmailForm :: (Monad m) => DF.Form [Text] m VerificationCode
verifyEmailForm = DF.text Nothing
