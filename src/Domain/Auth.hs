module Domain.Auth where

data Auth
  = Auth
      { authEmail    :: Email
      , authPassword :: Password
      }
  deriving (Eq, Show)

newtype Email
  = Email { emailRaw :: String } deriving (Eq, Ord, Show)

newtype Password
  = Password { passwordRaw :: String }
    deriving (Eq, Show)
