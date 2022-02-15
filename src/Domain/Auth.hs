{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module Domain.Auth where

import           Data.Text         ( Text )
import           Domain.Validation ( lengthBetween, regexMatches, validate )
import           Text.RawString.QQ ( r )

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
  , regexMatches [r|\d|] "Should contain a number"
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
