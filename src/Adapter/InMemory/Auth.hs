module Adapter.InMemory.Auth where

import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import qualified Domain.Auth                   as D

data State = State
  { stateAuths              :: [(D.UserId, D.Auth)]
  , stateUniversifiedEmails :: Map D.VerificationCode D.Email
  , stateVerifiedEmails     :: Set D.Email
  , stateUserIdCouter       :: Int
  , stateNotifications      :: Map D.Email D.VerificationCode
  , stateSessions           :: Map D.SessionId D.UserId
  }
  deriving (Show, Eq)

initialState :: State
initialState = State { stateAuths              = []
                     , stateUniversifiedEmails = mempty
                     , stateVerifiedEmails     = mempty
                     , stateUserIdCouter       = 0
                     , stateNotifications      = mempty
                     , stateSessions           = mempty
                     }

