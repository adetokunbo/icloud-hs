{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Network.ICloud.Internal.LoginFSM
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides datatypes that represent the Finite State Machine that specifies the
Login process
-}
module Network.ICloud.Internal.LoginFSM where

import Data.Functor ((<&>))
import Data.Kind (Type)
import Network.ICloud.Auth (Credentials, SessionData)


{- |
Module      : Network.ICloud.Internal.LoginFSM
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}

-- | Represents different outcomes of the login process.
data AtEnd
  = Normal Credentials
  | Halted
  deriving (Eq)


{- | @LoginEvent@ represents the valid events of the Login FSM.

Each event is represent by a typeclass function that is constrained
to go between valid states
-}
class LoginEvent m where
  -- | Represents valid finite states at the endpoints of a 'LoginEvent'
  type State m :: Type -> Type


  initial :: m (State m RatifyCredentials)
  ratifyCreds :: State m RatifyCredentials -> m (AfterCredentials (State m))
  ratifyArtifactDir :: State m RatifyArtifactDir -> m (AfterArtifactDir (State m))
  mkArtifactDir :: State m MkArtificatDir -> m (AfterMkArtifactDir (State m))
  mkClientId :: State m MakeClientId -> m (State m ReadyToAuth)
  loadSession :: State m LoadLastSession -> m (AfterLoadLastSession (State m))
  end :: BeforeEnd (State m) -> m AtEnd


-- | The canonical login process using events from 'LoginEvent'.
loginProcess ::
  ( LoginEvent m
  , Monad m
  ) =>
  m (BeforeEnd (State m))
loginProcess =
  initial >>= ratifyCreds >>= \case
    NoCreds e -> pure $ EndedAfterCredentials e
    GotCreds x -> onCredsLoaded x


onCredsLoaded ::
  (Monad m, LoginEvent m) =>
  State m RatifyArtifactDir ->
  m (BeforeEnd (State m))
onCredsLoaded s =
  ratifyArtifactDir s >>= \case
    DirPresent x -> onArtifactDirPresent x
    DirAbsent a ->
      mkArtifactDir a >>= \case
        NotMade e -> pure $ EndedAfterMkArtifactDir e
        DirMade x -> onArtifactDirPresent x


onArtifactDirPresent ::
  (Monad m, LoginEvent m) =>
  State m LoadLastSession ->
  m (BeforeEnd (State m))
onArtifactDirPresent s =
  loadSession s >>= \case
    NeedsClientId x -> mkClientId x <&> EndedReady
    HasClientId x -> pure $ EndedReady x


{- | The states of FSM defining the login process.

Each constructor specifies the concrete data required by the process in that
state, and is tagged with a distinct phantom type.
-}
data LoginFSM s where
  RatifyCredentials :: LoginFSM RatifyCredentials
  HaltMissingCredentials :: LoginFSM HaltMissingCredentials
  RatifyArtificatDir :: Credentials -> LoginFSM RatifyArtifactDir
  MkArtificatDir :: Credentials -> LoginFSM MkArtificatDir
  HaltCannotMkArtifactDir :: Credentials -> LoginFSM HaltCannotMkArtifactDir
  LoadLastSession :: Credentials -> LoginFSM LoadLastSession
  MakeClientId :: Credentials -> SessionData -> LoginFSM MakeClientId
  ReadyToAuth :: Credentials -> SessionData -> LoginFSM ReadyToAuth


-- | Phantom type linked to a unique state in 'LoginFSM'
data RatifyCredentials


-- | Phantom type linked to a unique state in 'LoginFSM'
data HaltMissingCredentials


-- | Phantom type linked to a unique state in 'LoginFSM'
data RatifyArtifactDir


-- | Phantom type linked to a unique state in 'LoginFSM'
data MkArtificatDir


-- | Phantom type linked to a unique state in 'LoginFSM'
data HaltCannotMkArtifactDir


-- | Phantom type linked to a unique state in 'LoginFSM'
data LoadLastSession


-- | Phantom type linked to a unique state in 'LoginFSM'
data MakeClientId


-- | Phantom type linked to a unique state in 'LoginFSM'
data ReadyToAuth


-- | The valid states to end from
data BeforeEnd f
  = EndedReady (f ReadyToAuth)
  | EndedAfterCredentials (f HaltMissingCredentials)
  | EndedAfterMkArtifactDir (f HaltCannotMkArtifactDir)


-- | The valid states after 'loadSession'
data AfterLoadLastSession f
  = NeedsClientId (f MakeClientId)
  | HasClientId (f ReadyToAuth)


-- | The valid states after 'mkArtifactDir'
data AfterMkArtifactDir f
  = NotMade (f HaltCannotMkArtifactDir)
  | DirMade (f LoadLastSession)


-- | The valid states after 'ratifyArtifactDir'
data AfterArtifactDir f
  = DirPresent (f LoadLastSession)
  | DirAbsent (f MkArtificatDir)


-- | The valid states after 'ratifyCreds'
data AfterCredentials f
  = NoCreds (f HaltMissingCredentials)
  | GotCreds (f RatifyArtifactDir)
