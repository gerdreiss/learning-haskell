module Adapter.HTTP.API.Auth where

import           ClassyPrelude
import           Domain.Auth
import           Text.Digestive.Form
import           Text.Digestive.Types

authForm :: (Monad m) => Form [Text] m Auth
authForm = Auth <$> "email" .: emailForm <*> "password" .: passwordForm
 where
  emailForm    = validate (toResult . mkEmail) (text Nothing)
  passwordForm = validate (toResult . mkPassword) (text Nothing)

toResult :: Either e a -> Result e a
toResult = either Error Success

verifyEmailForm :: (Monad m) => Form [Text] m VerificationCode
verifyEmailForm = text Nothing
