{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
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
import Data.Text (Text)
import Network.ICloud.Http.Internal (SrpContext (..))
import Network.ICloud.Session (AccountData, Credentials, SavedHeaders)
import Network.ICloud.Trust (Setup2SADevice, TrustData)


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
  mkArtifactDir :: State m MkArtifactDir -> m (AfterMkArtifactDir (State m))
  loadSession :: State m LoadLastSession -> m (AfterLoadLastSession (State m))
  validateSession :: State m HasSavedSession -> m (AfterValidateSession (State m))
  srpInit :: State m ReadyToAuth -> m (State m SrpInitDone)
  srpComplete :: State m SrpInitDone -> m (AfterSrpComplete (State m))
  increaseTrust :: State m IncreaseTrust -> m (State m DoAccountLogin)
  acctLogin :: State m DoAccountLogin -> m (AfterAcctLogin (State m))
  listTwoSaDevices :: State m NeedsTwoSa -> m (State m TwoSaReady)
  beginTwoFa :: State m ReadyForTwoFa -> m (State m TwoFaVerifying)
  verifyTwoFa :: State m TwoFaVerifying -> m (AfterTwoFaVerify (State m))
  beginTwoSa :: State m ReadyForTwoSa -> m (State m TwoSaVerifying)
  verifyTwoSa :: State m TwoSaVerifying -> m (AfterTwoSaVerify (State m))


-- | The outcome of 'loginProcess'.
data LoginOutcome f
  = LoginAuthenticated (f AuthComplete)
  | LoginNeedsTwoFa (f NeedsTwoFa)
  | LoginNeedsTwoSa (f TwoSaReady)
  | LoginHaltCreds (f HaltMissingCredentials)
  | LoginHaltDir (f HaltCannotMkArtifactDir)
  | LoginHaltSrp (f HaltInvalidSrp)


-- | The outcome of 'twoFaProcess' and 'twoSaProcess'.
data CompletionOutcome f
  = CompletionAuthenticated (f AuthComplete)
  | CompletionNeedsTwoSa (f TwoSaReady)


-- | The canonical login process using events from 'LoginEvent'.
loginProcess
  :: ( LoginEvent m
     , Monad m
     )
  => m (LoginOutcome (State m))
loginProcess =
  initial >>= ratifyCreds >>= \case
    NoCreds e -> pure $ LoginHaltCreds e
    GotCreds x -> onCredsLoaded x


onCredsLoaded
  :: (Monad m, LoginEvent m)
  => State m RatifyArtifactDir
  -> m (LoginOutcome (State m))
onCredsLoaded s =
  ratifyArtifactDir s >>= \case
    DirPresent x -> onArtifactDirPresent x
    DirAbsent a ->
      mkArtifactDir a >>= \case
        NotMade e -> pure $ LoginHaltDir e
        DirMade x -> onArtifactDirPresent x


onArtifactDirPresent
  :: (Monad m, LoginEvent m)
  => State m LoadLastSession
  -> m (LoginOutcome (State m))
onArtifactDirPresent s =
  loadSession s >>= \case
    HasClientId x -> onReadyToAuth x
    HasPriorSession x ->
      validateSession x >>= \case
        SessionStillValid y -> pure $ LoginAuthenticated y
        SessionStale y -> onReadyToAuth y


onReadyToAuth
  :: (Monad m, LoginEvent m)
  => State m ReadyToAuth
  -> m (LoginOutcome (State m))
onReadyToAuth s =
  srpInit s >>= srpComplete >>= \case
    SrpCompleteOk x -> increaseTrust x >>= acctLogin >>= fmap completionToLogin . onAcctLoginDone
    SrpComplete2FA x -> pure $ LoginNeedsTwoFa x
    SrpCompleteInvalidKey x -> pure $ LoginHaltSrp x


onAcctLoginDone
  :: (Monad m, LoginEvent m)
  => AfterAcctLogin (State m)
  -> m (CompletionOutcome (State m))
onAcctLoginDone = \case
  AcctLoginOk y -> pure $ CompletionAuthenticated y
  AcctLogin2SA y -> listTwoSaDevices y <&> CompletionNeedsTwoSa


completionToLogin :: CompletionOutcome f -> LoginOutcome f
completionToLogin (CompletionAuthenticated x) = LoginAuthenticated x
completionToLogin (CompletionNeedsTwoSa x) = LoginNeedsTwoSa x


-- | The 2FA completion process using events from 'LoginEvent'.
twoFaProcess
  :: (LoginEvent m, Monad m)
  => State m ReadyForTwoFa
  -> m (CompletionOutcome (State m))
twoFaProcess s =
  beginTwoFa s >>= verifyTwoFa >>= \case
    TwoFaOk x -> acctLogin x >>= onAcctLoginDone
    TwoFaRetry x -> twoFaProcess x


-- | The 2SA completion process using events from 'LoginEvent'.
twoSaProcess
  :: (LoginEvent m, Monad m)
  => State m ReadyForTwoSa
  -> m (CompletionOutcome (State m))
twoSaProcess s =
  beginTwoSa s >>= verifyTwoSa >>= \case
    TwoSaOk x -> acctLogin x >>= onAcctLoginDone
    TwoSaRetry x -> twoSaProcess x


{- | The states of FSM defining the login process.

Each constructor specifies the concrete data required by the process in that
state, and is tagged with a distinct phantom type.
-}
data LoginFSM s where
  RatifyCredentials :: LoginFSM RatifyCredentials
  HaltMissingCredentials :: LoginFSM HaltMissingCredentials
  RatifyArtifactDir :: Credentials -> LoginFSM RatifyArtifactDir
  MkArtifactDir :: Credentials -> LoginFSM MkArtifactDir
  HaltCannotMkArtifactDir :: Credentials -> LoginFSM HaltCannotMkArtifactDir
  LoadLastSession :: Credentials -> LoginFSM LoadLastSession
  HasSavedSession :: Credentials -> SavedHeaders -> LoginFSM HasSavedSession
  ReadyToAuth :: Credentials -> SavedHeaders -> LoginFSM ReadyToAuth
  SrpInitDone :: Credentials -> SrpContext -> LoginFSM SrpInitDone
  IncreaseTrust :: Credentials -> LoginFSM IncreaseTrust
  DoAccountLogin :: Credentials -> LoginFSM DoAccountLogin
  AuthComplete :: Credentials -> AccountData -> LoginFSM AuthComplete
  NeedsTwoFa :: Credentials -> TrustData -> LoginFSM NeedsTwoFa
  ReadyForTwoFa :: Credentials -> TrustData -> IO Text -> LoginFSM ReadyForTwoFa
  TwoFaVerifying :: Credentials -> TrustData -> IO Text -> LoginFSM TwoFaVerifying
  NeedsTwoSa :: Credentials -> LoginFSM NeedsTwoSa
  TwoSaReady :: Credentials -> [Setup2SADevice] -> LoginFSM TwoSaReady
  ReadyForTwoSa :: Credentials -> [Setup2SADevice] -> ([Setup2SADevice] -> IO Setup2SADevice) -> IO Text -> LoginFSM ReadyForTwoSa
  TwoSaVerifying :: Credentials -> Setup2SADevice -> [Setup2SADevice] -> ([Setup2SADevice] -> IO Setup2SADevice) -> IO Text -> LoginFSM TwoSaVerifying
  HaltInvalidSrp :: Credentials -> LoginFSM HaltInvalidSrp


-- | Phantom type linked to a unique state in 'LoginFSM'
data RatifyCredentials


-- | Phantom type linked to a unique state in 'LoginFSM'
data HaltMissingCredentials


-- | Phantom type linked to a unique state in 'LoginFSM'
data RatifyArtifactDir


-- | Phantom type linked to a unique state in 'LoginFSM'
data MkArtifactDir


-- | Phantom type linked to a unique state in 'LoginFSM'
data HaltCannotMkArtifactDir


-- | Phantom type linked to a unique state in 'LoginFSM'
data LoadLastSession


-- | Phantom type linked to a unique state in 'LoginFSM'
data HasSavedSession


-- | Phantom type linked to a unique state in 'LoginFSM'
data ReadyToAuth


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
data ReadyForTwoFa


-- | Phantom type linked to a unique state in 'LoginFSM'
data TwoFaVerifying


-- | Phantom type linked to a unique state in 'LoginFSM'
data NeedsTwoSa


-- | Phantom type linked to a unique state in 'LoginFSM'
data TwoSaReady


-- | Phantom type linked to a unique state in 'LoginFSM'
data ReadyForTwoSa


-- | Phantom type linked to a unique state in 'LoginFSM'
data TwoSaVerifying


-- | Phantom type linked to a unique state in 'LoginFSM'
data HaltInvalidSrp


-- | The valid states after 'loadSession'
data AfterLoadLastSession f
  = HasClientId (f ReadyToAuth)
  | HasPriorSession (f HasSavedSession)


-- | The valid states after 'validateSession'
data AfterValidateSession f
  = SessionStillValid (f AuthComplete)
  | SessionStale (f ReadyToAuth)


-- | The valid states after 'mkArtifactDir'
data AfterMkArtifactDir f
  = NotMade (f HaltCannotMkArtifactDir)
  | DirMade (f LoadLastSession)


-- | The valid states after 'ratifyArtifactDir'
data AfterArtifactDir f
  = DirPresent (f LoadLastSession)
  | DirAbsent (f MkArtifactDir)


-- | The valid states after 'ratifyCreds'
data AfterCredentials f
  = NoCreds (f HaltMissingCredentials)
  | GotCreds (f RatifyArtifactDir)


-- | The valid states after 'srpComplete'
data AfterSrpComplete f
  = SrpCompleteOk (f IncreaseTrust)
  | SrpComplete2FA (f NeedsTwoFa)
  | SrpCompleteInvalidKey (f HaltInvalidSrp)


-- | The valid states after 'acctLogin'
data AfterAcctLogin f
  = AcctLoginOk (f AuthComplete)
  | AcctLogin2SA (f NeedsTwoSa)


-- | The valid states after 'verifyTwoFa'
data AfterTwoFaVerify f
  = TwoFaOk (f DoAccountLogin)
  | TwoFaRetry (f ReadyForTwoFa)


-- | The valid states after 'verifyTwoSa'
data AfterTwoSaVerify f
  = TwoSaOk (f DoAccountLogin)
  | TwoSaRetry (f ReadyForTwoSa)
