{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module Domain.Auth (
  -- * Types
  Auth(..),
  Email,
  mkEmail,
  rawEmail,
  Password,
  mkPassword,
  rawPassword,
  UserId,
  VerificationCode,
  SessionId,
  RegistrationError(..),
  EmailVerificationError(..),
  LoginError(..),
  -- * Ports
  AuthRepo(..),
  EmailVerificationNotif(..),
  SessionRepo(..),
  -- * Use cases
  register,
  verifyEmail,
  login,
  resolveSessionId,
  getUser,
) where

import           Control.Monad.Except
    ( ExceptT (ExceptT), MonadError (throwError), lift, runExceptT )
import           Data.Text            ( Text, unpack )
import           Domain.Validation    ( lengthBetween, regexMatches, validate )
import           Text.RawString.QQ    ( r )

newtype Email
  = Email { emailRaw :: Text }
  deriving (Eq, Ord, Show)

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email
  [ regexMatches
      [r|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
      "Not a valid email"
  ]

rawEmail :: Email -> Text
rawEmail = emailRaw

newtype Password
  = Password { passwordRaw :: Text }
  deriving (Eq, Show)

mkPassword :: Text -> Either [Text] Password
mkPassword pass = validate Password
  [ lengthBetween 5 50 "Should be between 5 and 50"
  , regexMatches [r|[0-9]|] "Should contain a number"
  , regexMatches [r|[A-Z]|] "Should contain a uppercase letter"
  , regexMatches [r|[a-z]|] "Should contain a lowercase letter"
  ]
  pass

rawPassword :: Password -> Text
rawPassword = passwordRaw

data Auth
  = Auth
      { authEmail    :: Email
      , authPassword :: Password
      }
  deriving (Eq, Show)


data EmailVerificationError
  = EmailVerificationErrorInvalidCode
  deriving (Eq, Show)
data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Eq, Show)
data PasswordValidationErr
  = PasswordValidationErrLength Int
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber
  deriving (Eq, Show)
data RegistrationError
  = RegistrationErrorEmailToken
  deriving (Eq, Show)

type VerificationCode = Text
type UserId = Int
type SessionId = Text

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing         -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _)   -> lift $ newSession uId

register :: (AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  vCode <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode

verifyEmail :: AuthRepo m
            => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = setEmailAsVerified

-- instance AuthRepo IO where
--   addAuth (Auth email pass) = do
--     putStrLn . unpack $ "adding auth: " <> rawEmail email
--     return $ Right "fake verification code"

-- instance EmailVerificationNotif IO where
--   notifyEmailVerification email vCode =
--     putStrLn . unpack $ "Notify " <> rawEmail email <> " - " <> vCode
