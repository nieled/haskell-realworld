module Domain.Auth where

import           Data.Text ( Text )

data Auth
  = Auth
      { authEmail    :: Email
      , authPassword :: Password
      }
  deriving (Eq, Show)

newtype Email
  = Email { emailRaw :: Text }
  deriving (Eq, Ord, Show)

newtype Password
  = Password { passwordRaw :: Text }
  deriving (Eq, Show)
