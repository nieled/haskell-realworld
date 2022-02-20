{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Adapter.InMemory.Auth where

import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , readTVar
                                                , readTVarIO
                                                , writeTVar
                                                )
import           Control.Monad.List
import           Control.Monad.Reader           ( MonadReader )
import           Control.Monad.Reader.Class     ( asks )
import           Data.Has
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Domain.Auth                   as D
import           Domain.Auth                    ( AuthRepo(addAuth) )
import           Text.StringRandom              ( stringRandomIO )

data State = State
  { stateAuths              :: [(D.UserId, D.Auth)]
  , stateUniversifiedEmails :: M.Map D.VerificationCode D.Email
  , stateVerifiedEmails     :: S.Set D.Email
  , stateUserIdCouter       :: Int
  , stateNotifications      :: M.Map D.Email D.VerificationCode
  , stateSessions           :: M.Map D.SessionId D.UserId
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

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addAuth
  :: InMemory r m => D.Auth -> m (Either D.RegistrationError D.VerificationCode)
addAuth = undefined

setEmailAsVerified
  :: InMemory r m
  => D.VerificationCode
  -> m (Either D.EmailVerificationError ())
setEmailAsVerified = undefined

findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth = undefined

findEmailFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId = undefined

notifyEmailVerification
  :: InMemory r m => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification = undefined

newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession uId = do
  tvar <- asks getter
  sId <- liftIO $ ((pack . show $ uId) <>) <$> stringRandomIO "[A-Z-a-z0-9]{16}"
  atomically $ do
    state <- readTVar tvar
    let sessions    = stateSessions state
        newSessions = M.insert sId uId sessions
        newState    = state { stateSessions = newSessions }
    writeTVar tvar newState
    return sId




findUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  liftIO $ M.lookup sId . stateSessions <$> readTVarIO tvar
