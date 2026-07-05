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

import Data.Kind (Type)
import Network.ICloud.Session (AccountData, Credentials, SavedHeaders, SrpContext)
import Network.ICloud.Trust (Setup2SADevice, TrustData)


-- | Represents different outcomes of the login process.
data AtEnd
  = Normal Credentials AccountData
  | Needs2FA Credentials TrustData
  | Needs2SA Credentials [Setup2SADevice]
  | Halted


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
  srpInit :: State m ReadyToAuth -> m (State m SrpInitDone)
  srpComplete :: State m SrpInitDone -> m (AfterSrpComplete (State m))
  increaseTrust :: State m IncreaseTrust -> m (State m DoAccountLogin)
  acctLogin :: State m DoAccountLogin -> m (AfterAcctLogin (State m))
  end :: BeforeEnd (State m) -> m AtEnd


-- | The canonical login process using events from 'LoginEvent'.
loginProcess
  :: ( LoginEvent m
     , Monad m
     )
  => m (BeforeEnd (State m))
loginProcess =
  initial >>= ratifyCreds >>= \case
    NoCreds e -> pure $ EndedAfterCredentials e
    GotCreds x -> onCredsLoaded x


onCredsLoaded
  :: (Monad m, LoginEvent m)
  => State m RatifyArtifactDir
  -> m (BeforeEnd (State m))
onCredsLoaded s =
  ratifyArtifactDir s >>= \case
    DirPresent x -> onArtifactDirPresent x
    DirAbsent a ->
      mkArtifactDir a >>= \case
        NotMade e -> pure $ EndedAfterMkArtifactDir e
        DirMade x -> onArtifactDirPresent x


onArtifactDirPresent
  :: (Monad m, LoginEvent m)
  => State m LoadLastSession
  -> m (BeforeEnd (State m))
onArtifactDirPresent s =
  loadSession s >>= \case
    NeedsClientId x -> mkClientId x >>= onReadyToAuth
    HasClientId x -> onReadyToAuth x
    SessionStillValid x -> pure $ EndedAuthenticated x


onReadyToAuth
  :: (Monad m, LoginEvent m)
  => State m ReadyToAuth
  -> m (BeforeEnd (State m))
onReadyToAuth s =
  srpInit s >>= srpComplete >>= \case
    SrpComplete2FA x -> pure $ EndedNeedsTwoFa x
    SrpCompleteOk x ->
      increaseTrust x >>= acctLogin >>= \case
        AcctLoginOk y -> pure $ EndedAuthenticated y
        AcctLogin2SA y -> pure $ EndedNeedsTwoSa y


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
  MakeClientId :: Credentials -> SavedHeaders -> LoginFSM MakeClientId
  ReadyToAuth :: Credentials -> SavedHeaders -> LoginFSM ReadyToAuth
  SrpInit :: Credentials -> SavedHeaders -> LoginFSM SrpInit
  SrpInitDone :: Credentials -> SrpContext -> LoginFSM SrpInitDone
  IncreaseTrust :: Credentials -> LoginFSM IncreaseTrust
  DoAccountLogin :: Credentials -> LoginFSM DoAccountLogin
  AuthComplete :: Credentials -> AccountData -> LoginFSM AuthComplete
  NeedsTwoFa :: Credentials -> TrustData -> LoginFSM NeedsTwoFa
  NeedsTwoSa :: Credentials -> [Setup2SADevice] -> LoginFSM NeedsTwoSa
  HaltInvalidSrp :: Credentials -> LoginFSM HaltInvalidSrp


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


-- | Phantom type linked to a unique state in 'LoginFSM'
data SrpInit


-- | Phantom type linked to a unique state in 'LoginFSM'
data SrpInitDone


-- | Phantom type linked to a unique state in 'LoginFSM'
data IncreaseTrust


-- | Phantom type linked to a unique state in 'LoginFSM'
data DoAccountLogin


-- | Phantom type linked to a unique state in 'LoginFSM'
data AuthComplete


-- | Phantom type linked to a unique state in 'LoginFSM'
data NeedsTwoFa


-- | Phantom type linked to a unique state in 'LoginFSM'
data NeedsTwoSa


-- | Phantom type linked to a unique state in 'LoginFSM'
data HaltInvalidSrp


-- | The valid states to end from
data BeforeEnd f
  = EndedAfterCredentials (f HaltMissingCredentials)
  | EndedAfterMkArtifactDir (f HaltCannotMkArtifactDir)
  | EndedAuthenticated (f AuthComplete)
  | EndedNeedsTwoFa (f NeedsTwoFa)
  | EndedNeedsTwoSa (f NeedsTwoSa)
  | EndedHaltInvalidSrp (f HaltInvalidSrp)


-- | The valid states after 'loadSession'
data AfterLoadLastSession f
  = NeedsClientId (f MakeClientId)
  | HasClientId (f ReadyToAuth)
  | SessionStillValid (f AuthComplete)


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


-- | The valid states after 'srpComplete'
data AfterSrpComplete f
  = SrpCompleteOk (f IncreaseTrust)
  | SrpComplete2FA (f NeedsTwoFa)


-- | The valid states after 'acctLogin'
data AfterAcctLogin f
  = AcctLoginOk (f AuthComplete)
  | AcctLogin2SA (f NeedsTwoSa)
