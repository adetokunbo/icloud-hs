{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}

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
import Data.Word (Word8)
import Network.ICloud.Internal.Http (SrpContext (..))
import Network.ICloud.Internal.Session (AccountData, Credentials, SavedHeaders)
import Network.ICloud.Internal.Trust (Setup2SADevice, TrustData, TrustedPhone)


-- | Configuration for the 2FA challenge process.
data TwoFaConfig = TwoFaConfig
  { tfcPickPhone :: TrustData -> IO (Maybe TrustedPhone)
  -- ^ select a phone to receive an SMS code, or 'Nothing' to use a trusted device push
  , tfcReadCode :: Word8 -> IO Text
  -- ^ prompt the user for the verification code; receives the expected digit count
  }


-- | Configuration for the 2SA challenge process.
data TwoSaConfig = TwoSaConfig
  { tscPickDevice :: [Setup2SADevice] -> IO Setup2SADevice
  -- ^ select the device to receive a verification code
  , tscReadCode :: IO Text
  -- ^ prompt the user for the verification code
  }


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
  acctLogin :: State m DoAccountLogin -> m (AfterAcctLogin (State m))
  listTwoSaDevices :: State m NeedsTwoSa -> m (State m TwoSaReady)
  beginTwoFa :: State m ReadyForTwoFa -> TwoFaConfig -> m (State m TwoFaVerifying)
  verifyTwoFa :: State m TwoFaVerifying -> TwoFaConfig -> m (AfterTwoFaVerify (State m))
  doTrust :: State m DoTrust -> m (State m DoAccountLogin)
  beginTwoSa :: State m ReadyForTwoSa -> TwoSaConfig -> m (State m TwoSaVerifying)
  verifyTwoSa :: State m TwoSaVerifying -> TwoSaConfig -> m (AfterTwoSaVerify (State m))


-- | The outcome of 'loginProcess'.
data LoginOutcome f
  = LoginAuthenticated (f AuthComplete)
  | LoginNeedsTwoFa (f NeedsTwoFa)
  | LoginNeedsTwoSa (f TwoSaReady)
  | LoginHaltCreds (f HaltMissingCredentials)
  | LoginHaltDir (f HaltCannotMkArtifactDir)
  | LoginHaltSrp (f HaltInvalidSrp)
  | LoginHaltTwoFaLocked (f HaltTwoFaLocked)


-- | The outcome of 'twoFaProcess' and 'twoSaProcess'.
data CompletionOutcome f
  = CompletionAuthenticated (f AuthComplete)
  | CompletionNeedsTwoFa (f NeedsTwoFa)
  | CompletionNeedsTwoSa (f TwoSaReady)
  | CompletionTwoFaLocked (f HaltTwoFaLocked)


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
    SrpCompleteOk x -> acctLogin x >>= fmap completionToLogin . onAcctLoginDone
    SrpCompleteInvalidKey x -> pure $ LoginHaltSrp x


onAcctLoginDone
  :: (Monad m, LoginEvent m)
  => AfterAcctLogin (State m)
  -> m (CompletionOutcome (State m))
onAcctLoginDone = \case
  AcctLoginOk y -> pure $ CompletionAuthenticated y
  AcctLogin2FA y -> pure $ CompletionNeedsTwoFa y
  AcctLogin2SA y -> listTwoSaDevices y <&> CompletionNeedsTwoSa


completionToLogin :: CompletionOutcome f -> LoginOutcome f
completionToLogin (CompletionAuthenticated x) = LoginAuthenticated x
completionToLogin (CompletionNeedsTwoFa x) = LoginNeedsTwoFa x
completionToLogin (CompletionNeedsTwoSa x) = LoginNeedsTwoSa x
completionToLogin (CompletionTwoFaLocked x) = LoginHaltTwoFaLocked x


-- | The 2FA completion process using events from 'LoginEvent'.
twoFaProcess
  :: (LoginEvent m, Monad m)
  => State m ReadyForTwoFa
  -> TwoFaConfig
  -> m (CompletionOutcome (State m))
twoFaProcess s cfg =
  beginTwoFa s cfg >>= flip verifyTwoFa cfg >>= \case
    TwoFaOk x -> doTrust x >>= acctLogin >>= onAcctLoginDone
    TwoFaRetry x -> twoFaProcess x cfg
    TwoFaLocked x -> pure $ CompletionTwoFaLocked x


-- | The 2SA completion process using events from 'LoginEvent'.
twoSaProcess
  :: (LoginEvent m, Monad m)
  => State m ReadyForTwoSa
  -> TwoSaConfig
  -> m (CompletionOutcome (State m))
twoSaProcess s cfg =
  beginTwoSa s cfg >>= flip verifyTwoSa cfg >>= \case
    TwoSaOk x -> acctLogin x >>= onAcctLoginDone
    TwoSaRetry x -> twoSaProcess x cfg


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
  DoAccountLogin :: Credentials -> LoginFSM DoAccountLogin
  AuthComplete :: Credentials -> AccountData -> LoginFSM AuthComplete
  NeedsTwoFa :: Credentials -> LoginFSM NeedsTwoFa
  ReadyForTwoFa :: Credentials -> TrustData -> LoginFSM ReadyForTwoFa
  TwoFaVerifying :: Credentials -> TrustData -> Maybe TrustedPhone -> LoginFSM TwoFaVerifying
  DoTrust :: Credentials -> LoginFSM DoTrust
  NeedsTwoSa :: Credentials -> LoginFSM NeedsTwoSa
  TwoSaReady :: Credentials -> [Setup2SADevice] -> LoginFSM TwoSaReady
  ReadyForTwoSa :: Credentials -> [Setup2SADevice] -> LoginFSM ReadyForTwoSa
  TwoSaVerifying :: Credentials -> Setup2SADevice -> [Setup2SADevice] -> LoginFSM TwoSaVerifying
  HaltInvalidSrp :: Credentials -> LoginFSM HaltInvalidSrp
  HaltTwoFaLocked :: Credentials -> LoginFSM HaltTwoFaLocked


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
data DoTrust


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


-- | Phantom type linked to a unique state in 'LoginFSM'
data HaltTwoFaLocked


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
  = SrpCompleteOk (f DoAccountLogin)
  | SrpCompleteInvalidKey (f HaltInvalidSrp)


-- | The valid states after 'acctLogin'
data AfterAcctLogin f
  = AcctLoginOk (f AuthComplete)
  | AcctLogin2FA (f NeedsTwoFa)
  | AcctLogin2SA (f NeedsTwoSa)


-- | The valid states after 'verifyTwoFa'
data AfterTwoFaVerify f
  = TwoFaOk (f DoTrust)
  | TwoFaRetry (f ReadyForTwoFa)
  | TwoFaLocked (f HaltTwoFaLocked)


-- | The valid states after 'verifyTwoSa'
data AfterTwoSaVerify f
  = TwoSaOk (f DoAccountLogin)
  | TwoSaRetry (f ReadyForTwoSa)
