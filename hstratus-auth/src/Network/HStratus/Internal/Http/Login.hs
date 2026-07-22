{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}

module Network.HStratus.Internal.Http.Login
  ( -- * Login state
    AuthState (..)

    -- * Login
  , login
  , loginWith

    -- * Completing two-factor challenges
  , completeTwoFactor
  , completeTwoFactorWith

    -- * Completing two-step challenges
  , complete2SA
  , complete2SAWith
  )
where

import Control.Exception (IOException, catch, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import Crypto.SRP (calcResults, mkFromClient)
import Data.Aeson (FromJSON (..))
import Data.Aeson.Types (Value (..), parseEither)
import qualified Data.Text as Text
import Data.Word (Word8)
import Network.HStratus.Internal.Http (SrpContext (..))
import Network.HStratus.Internal.Http.Api
  ( Api (..)
  , AuthCode
  )
import Network.HStratus.Internal.Http.Signin
  ( accountLogin
  , doTrustStep
  , fetchTrustData
  , listSetupDevices
  , requestSmsCode
  , runSigninComplete
  , runSigninInit
  , sendSetupVerification
  , triggerTwoFaPush
  , validate
  , validateSetupVerification
  , verifySmsCode
  , verifyTwoFaCode
  )
import Network.HStratus.Internal.HttpErrors (AuthError (..))
import Network.HStratus.Internal.LoginFSM
  ( AfterAcctLogin (..)
  , AfterArtifactDir (..)
  , AfterCredentials (..)
  , AfterLoadLastSession (..)
  , AfterMkArtifactDir (..)
  , AfterSrpComplete (..)
  , AfterTwoFaVerify (..)
  , AfterTwoSaVerify (..)
  , AfterValidateSession (..)
  , CompletionOutcome (..)
  , LoginEvent (..)
  , LoginFSM (..)
  , LoginOutcome (..)
  , TwoFaConfig (..)
  , TwoSaConfig (..)
  , loginProcess
  , twoFaProcess
  , twoSaProcess
  )
import Network.HStratus.Internal.Session
  ( accountDataRequires2FA
  , accountDataRequires2SA
  , loadAccountData
  , loadSavedHeaders
  , pristine
  , saveAccountData
  , saveLoginMsg
  , unknownAccountData
  )
import Network.HStratus.Internal.Trust
  ( CodeStatus (..)
  , TrustData (..)
  , TrustedPhone
  , pleaseReadCode
  , selectSetupDevice
  , selectTwoFaPhone
  )
import Network.HStratus.Session (AccountData (..), Credentials (..), Session (..))
import Network.HStratus.Trust (Setup2SADevice (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)


newtype LoginM a = LoginM {runLoginM :: ReaderT Api IO a}
  deriving (Functor, Applicative, Monad, MonadIO)


ask :: LoginM Api
ask = LoginM Reader.ask


asks :: (Api -> b) -> LoginM b
asks = LoginM . Reader.asks


{- | The result of a login attempt.

'login' and 'loginWith' return only 'Authenticated'; 2FA and 2SA challenges
are resolved internally.  'Requires2FA' and 'Requires2SA' are only produced by
'completeTwoFactor', 'completeTwoFactorWith', 'complete2SA', and 'complete2SAWith'.
-}
data AuthState
  = -- | Sign-in succeeded; the 'Session' is refreshed and 'AccountData' is available.
    Authenticated Session AccountData
  | -- | Sign-in requires a two-factor code; use 'completeTwoFactor' or 'completeTwoFactorWith' to proceed.
    Requires2FA Session
  | -- | Sign-in requires a legacy two-step code; use 'complete2SA' or 'complete2SAWith' to proceed.
    Requires2SA Session [Setup2SADevice]


instance Show AuthState where
  show (Authenticated _ ad) = "Authenticated <session> " ++ show ad
  show (Requires2FA _) = "Requires2FA <session>"
  show (Requires2SA _ ds) = "Requires2SA <session> " ++ show ds


-- | Logs into ICloud, completing any 2FA or 2SA challenge automatically
login :: Api -> IO AuthState
login = loginWith pleaseReadCode selectTwoFaPhone selectSetupDevice


-- | Like 'login' with injectable code prompt, phone selector, and device selector, for testing
loginWith
  :: (Word8 -> IO AuthCode)
  -> (TrustData -> IO (Maybe TrustedPhone))
  -> ([Setup2SADevice] -> IO Setup2SADevice)
  -> Api
  -> IO AuthState
loginWith readCode pickPhone pickDevice api =
  runReaderT (runLoginM loginProcess) api >>= \case
    LoginAuthenticated (AuthComplete _ ad) -> pure $ Authenticated (apiSession api) ad
    LoginNeedsTwoFa (NeedsTwoFa _) -> completeTwoFactorWith readCode pickPhone api
    LoginNeedsTwoSa (TwoSaReady _ ds) -> complete2SAWith pickDevice (readCode 6) api ds
    LoginHaltCreds _ -> throwIO CredentialsMissing
    LoginHaltDir _ -> throwIO $ ArtifactDirCreationFailed (sessionTopDir (apiSession api))
    LoginHaltSrp _ -> throwIO SrpProtocolError
    LoginHaltTwoFaLocked _ -> throwIO TwoFactorLocked


instance LoginEvent LoginM where
  type State LoginM = LoginFSM


  initial = pure RatifyCredentials


  ratifyCreds RatifyCredentials =
    asks (GotCreds . RatifyArtifactDir . sessionCreds . apiSession)


  ratifyArtifactDir (RatifyArtifactDir creds) = do
    dir <- sessionTopDir . apiSession <$> ask
    exists <- liftIO $ doesDirectoryExist dir
    pure $
      if exists
        then DirPresent $ LoadLastSession creds
        else DirAbsent $ MkArtifactDir creds


  mkArtifactDir (MkArtifactDir creds) = do
    dir <- sessionTopDir . apiSession <$> ask
    ok <- liftIO $ (createDirectoryIfMissing True dir >> pure True) `catch` (\(_ :: IOException) -> pure False)
    pure $
      if ok
        then DirMade $ LoadLastSession creds
        else NotMade $ HaltCannotMkArtifactDir creds


  loadSession (LoadLastSession creds) = do
    savedHdrs <- ask >>= liftIO . loadSavedHeaders . apiSession
    pure $
      if savedHdrs == pristine
        then HasClientId $ ReadyToAuth creds savedHdrs
        else HasPriorSession $ HasSavedSession creds savedHdrs


  validateSession (HasSavedSession creds savedHdrs) = do
    valid <- ask >>= liftIO . validate
    if not valid
      then pure $ SessionStale $ ReadyToAuth creds savedHdrs
      else do
        mbAd <- ask >>= liftIO . loadAccountData . apiSession
        pure $ case mbAd of
          Just ad | accountDataRequires2FA ad -> SessionStale $ ReadyToAuth creds savedHdrs
          Just ad | accountDataRequires2SA ad -> SessionStale $ ReadyToAuth creds savedHdrs
          Just ad -> SessionStillValid $ AuthComplete creds ad
          Nothing -> SessionStillValid $ AuthComplete creds unknownAccountData


  srpInit (ReadyToAuth creds _) = do
    api <- ask
    let user = credAccountName creds
        pass = credPassword creds
    fc <- liftIO $ mkFromClient user pass (apiGroup api)
    (fs, kd) <- liftIO $ runSigninInit api fc
    pure $ SrpInitDone creds (SrpContext fc fs kd)


  srpComplete (SrpInitDone creds ctx) = do
    api <- ask
    let SrpContext{srpFromClient = fc, srpFromServer = fs, srpKeyDeriver = kd} = ctx
    case calcResults kd fc fs of
      Nothing -> pure $ SrpCompleteInvalidKey $ HaltInvalidSrp creds
      Just results -> do
        liftIO $ runSigninComplete api kd results
        pure $ SrpCompleteOk $ DoAccountLogin creds


  acctLogin (DoAccountLogin creds) = do
    api <- ask
    loginReply <- liftIO $ accountLogin api
    ad <- liftIO $ parseAccountData loginReply
    liftIO $ saveLoginMsg (apiSession api) loginReply
    liftIO $ saveAccountData (apiSession api) ad
    pure $
      if
        | accountDataRequires2SA ad -> AcctLogin2SA $ NeedsTwoSa creds
        | accountDataRequires2FA ad -> AcctLogin2FA $ NeedsTwoFa creds
        | otherwise -> AcctLoginOk $ AuthComplete creds ad


  listTwoSaDevices (NeedsTwoSa creds) = do
    ask >>= fmap (TwoSaReady creds) . liftIO . listSetupDevices


  beginTwoFa (ReadyForTwoFa creds td) TwoFaConfig{tfcPickPhone} = do
    mbPhone <- liftIO $ tfcPickPhone td
    case mbPhone of
      Nothing -> ask >>= liftIO . triggerTwoFaPush
      Just phone -> ask >>= liftIO . flip requestSmsCode phone
    pure $ TwoFaVerifying creds td mbPhone


  verifyTwoFa (TwoFaVerifying creds td mbPhone) TwoFaConfig{tfcReadCode} = do
    api <- ask
    code <- liftIO $ tfcReadCode (scLength (tdSecurityCode td))
    ok <- liftIO $ case mbPhone of
      Nothing -> verifyTwoFaCode api code
      Just phone -> verifySmsCode api phone code
    if ok
      then pure $ TwoFaOk $ DoTrust creds
      else do
        freshTd <- liftIO $ fetchTrustData api
        let cs = tdSecurityCode freshTd
        pure $
          if scTooManyCodesValidated cs || scSecurityCodeLocked cs || scSecurityCodeCooldown cs
            then TwoFaLocked $ HaltTwoFaLocked creds
            else TwoFaRetry $ ReadyForTwoFa creds freshTd


  doTrust (DoTrust creds) = do
    ask >>= liftIO . doTrustStep
    pure $ DoAccountLogin creds


  beginTwoSa (ReadyForTwoSa creds devices) TwoSaConfig{tscPickDevice} = do
    api <- ask
    device <- liftIO $ tscPickDevice devices
    liftIO $ sendSetupVerification api device
    pure $ TwoSaVerifying creds device devices


  verifyTwoSa (TwoSaVerifying creds device devices) TwoSaConfig{tscReadCode} = do
    api <- ask
    code <- liftIO tscReadCode
    ok <- liftIO $ validateSetupVerification api device code
    pure $
      if ok
        then TwoSaOk $ DoAccountLogin creds
        else TwoSaRetry $ ReadyForTwoSa creds devices


parseAccountData :: Value -> IO AccountData
parseAccountData v =
  either (throwIO . UnexpectedResponse . Text.pack) pure $
    parseEither parseJSON v


-- | Complete a pending 2FA (auth-endpoint) challenge
completeTwoFactor :: Api -> IO AuthState
completeTwoFactor = completeTwoFactorWith pleaseReadCode (\_ -> pure Nothing)


-- | Like 'completeTwoFactor' with an injectable code prompt and phone selector, for testing
completeTwoFactorWith :: (Word8 -> IO AuthCode) -> (TrustData -> IO (Maybe TrustedPhone)) -> Api -> IO AuthState
completeTwoFactorWith readCode pickPhone api = do
  td <- fetchTrustData api
  let start = ReadyForTwoFa (sessionCreds (apiSession api)) td
      cfg = TwoFaConfig{tfcPickPhone = pickPhone, tfcReadCode = readCode}
  runReaderT (runLoginM (twoFaProcess start cfg)) api >>= \case
    CompletionAuthenticated (AuthComplete _ ad) -> pure $ Authenticated (apiSession api) ad
    CompletionNeedsTwoFa _ -> throwIO TwoFactorStillRequired
    CompletionNeedsTwoSa (TwoSaReady _ ds) -> pure $ Requires2SA (apiSession api) ds
    CompletionTwoFaLocked _ -> throwIO TwoFactorLocked


-- | Used when already holding a 'Requires2SA' result from 'completeTwoFactor' or 'completeTwoFactorWith'
complete2SA :: Api -> [Setup2SADevice] -> IO AuthState
complete2SA = complete2SAWith selectSetupDevice (pleaseReadCode 6)


-- | Like 'complete2SA' with injectable device selector and code prompt, for testing
complete2SAWith
  :: ([Setup2SADevice] -> IO Setup2SADevice)
  -> IO AuthCode
  -> Api
  -> [Setup2SADevice]
  -> IO AuthState
complete2SAWith pickDevice readCode api devices = do
  let start = ReadyForTwoSa (sessionCreds (apiSession api)) devices
      cfg = TwoSaConfig{tscPickDevice = pickDevice, tscReadCode = readCode}
  runReaderT (runLoginM (twoSaProcess start cfg)) api >>= \case
    CompletionAuthenticated (AuthComplete _ ad) -> pure $ Authenticated (apiSession api) ad
    CompletionNeedsTwoFa _ -> throwIO TwoFactorStillRequired
    CompletionNeedsTwoSa (TwoSaReady _ ds) -> pure $ Requires2SA (apiSession api) ds
    CompletionTwoFaLocked _ -> throwIO TwoFactorLocked
