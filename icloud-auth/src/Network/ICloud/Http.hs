{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Network.ICloud.Http
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

High-level HTTP client for the iCloud authentication API.

= Typical usage

Create an 'Api' handle with 'mkApi', choosing the 'Realm' that matches the
user's region.  Then call 'login' to run the full sign-in flow: SRP credential
exchange followed by any required two-factor ('completeTwoFactor') or two-step
('complete2SA') challenge, then the account-login request.  On success 'login'
returns 'Authenticated' carrying the refreshed 'Session' and 'AccountData'.

= Injectable alternatives

'login' resolves 2FA and 2SA challenges interactively using the prompts from
"Network.ICloud.Trust".  Pass your own code-reader and device-selector to
'loginWith' to bypass the interactive prompts — useful in tests or automation.

If you already hold a 'Requires2FA' or 'Requires2SA' result from a prior call,
resume the flow with 'completeTwoFactor' \/ 'completeTwoFactorWith' or
'complete2SA' \/ 'complete2SAWith'.
-}
module Network.ICloud.Http
  ( -- * API handle
    mkApi
  , mkApiWith

    -- * Login
  , login
  , loginWith

    -- * Fetching two-factor options
  , fetchTrustData

    -- * SMS phone code
  , requestSmsCode
  , verifySmsCode

    -- * Completing two-factor challenges
  , completeTwoFactor
  , completeTwoFactorWith

    -- * Completing two-step challenges
  , complete2SA
  , complete2SAWith

    -- * Types
  , Api
  , AuthState (..)
  , ApiLogger

    -- * Authenticated HTTP
  , rawRequest

    -- * Logging
  , withLogger
  , fileLogger
  , verboseLogger

    -- * Errors
  , AuthError (..)
  )
where

import Control.Exception (IOException, catch, throwIO)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.SRP
  ( FromClient (..)
  , FromServer (..)
  , KnownAlgorithm (SHA256)
  , PrimeGroup (G2048)
  , Results (..)
  , calcResults
  , digestSize
  , mkFromClient
  )
import Data.Aeson
  ( FromJSON (..)
  , Key
  , Object
  , eitherDecode
  , encode
  , withObject
  , (.:)
  )
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Types (Parser, Value (..), parseMaybe)
import Data.Base64.Types (extractBase64)
import Data.ByteString (ByteString, isPrefixOf)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (decodeBase64Untyped, encodeBase64)
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk, original)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Data.Word (Word64, Word8)
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , Response (..)
  , httpLbs
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
  ( HeaderName
  , RequestHeaders
  , Status (..)
  , hAccept
  , hContentType
  )
import Network.ICloud.Http.Endpoints (Endpoints (..), Realm, realmEndpoints)
import Network.ICloud.Internal.Endpoints
  ( accountLoginBase
  , extendPath
  , homeHeaders
  , listDevices
  , sendVerification
  , signinCompleteBase
  , signinInitBase
  , toPut
  , twoFaOptionsBase
  , twoSvTrust
  , validateBase
  , validateVerification
  , verifySecurityCodeReq
  , withAcceptJson
  , withAppleOauthHeaders
  , withBody
  , withHeaders
  , withICloudWidgetKey
  )
import Network.ICloud.Internal.Http
  ( KeyDeriver (..)
  , PasswordProtocol (..)
  , SrpContext (..)
  , hCounter
  , hSessionId
  , needsRetry
  , phoneCodeBody
  , phoneTriggerBody
  , validateSetupBody
  )
import Network.ICloud.Internal.HttpErrors
  ( ApiResponse
  , AuthError (..)
  , extractOr
  )
import Network.ICloud.Internal.LoginFSM
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
import Network.ICloud.Internal.PBKDF2 (FancyPseudoRandomF, wrapIO)
import Network.ICloud.Internal.Session
  ( SavedHeaders (..)
  , accountDataRequires2FA
  , accountDataRequires2SA
  , cookiePath
  , loadAccountData
  , loadSavedHeaders
  , pristine
  , saveAccountData
  , saveLoginMsg
  , unknownAccountData
  , updateSavedHeaders
  , updateSessionSavedHeaders
  )
import Network.ICloud.Internal.Trust
  ( CodeStatus (..)
  , TrustData (..)
  , TrustedPhone
  , pleaseReadCode
  , selectSetupDevice
  , selectTwoFaPhone
  )
import Network.ICloud.Session (AccountData (..), Credentials (..), Session (..))
import qualified Network.ICloud.Session as Session
import Network.ICloud.Trust (Setup2SADevice (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.IO (Handle, hPutStrLn)
import Web.Cookie.Jar (usingCookiesFromFile)


-- | A hook called after every HTTP response; receives the outgoing 'Request' and the 'Response'.
newtype ApiLogger = ApiLogger (Request -> Response LBS.ByteString -> IO ())


{- | Bundles the HTTP manager, session state, and endpoint configuration
needed to call the iCloud API.  Created by 'mkApi' or 'mkApiWith'.
-}
data Api = Api
  { apiManager :: !Manager
  , apiSession :: !Session
  , apiEndpoints :: !Endpoints
  , apiHashAlgorithm :: !KnownAlgorithm
  , apiWrappedPseudoRF :: !FancyPseudoRandomF
  , apiGroup :: !PrimeGroup
  , apiLogger :: !(Maybe ApiLogger)
  }


{- | Create an 'Api' using the endpoint set for the given 'Realm'.

Loads credentials and session state from disk via 'Network.ICloud.Session.loadSession'
and creates a new TLS manager.  Use 'mkApiWith' to supply a pre-built manager
and endpoint set — for example in tests.
-}
mkApi :: Realm -> IO Api
mkApi realm = do
  let apiHashAlgorithm = SHA256
      apiGroup = G2048
      apiEndpoints = realmEndpoints realm
      apiLogger = Nothing
  apiManager <- newTlsManager
  apiSession <- Session.loadSession
  apiWrappedPseudoRF <- wrapIO SHA256.hmac $ digestSize apiHashAlgorithm
  pure
    Api
      { apiGroup
      , apiEndpoints
      , apiManager
      , apiHashAlgorithm
      , apiSession
      , apiWrappedPseudoRF
      , apiLogger
      }


-- | Create an 'Api' from a pre-built 'Session', 'Endpoints', and 'Manager'. Use this when you need to supply your own HTTP manager or a custom endpoint set — for example in tests.
mkApiWith :: Session -> Endpoints -> Manager -> IO Api
mkApiWith apiSession apiEndpoints apiManager = do
  let apiHashAlgorithm = SHA256
      apiGroup = G2048
      apiLogger = Nothing
  apiWrappedPseudoRF <- wrapIO SHA256.hmac $ digestSize apiHashAlgorithm
  pure
    Api
      { apiGroup
      , apiEndpoints
      , apiManager
      , apiHashAlgorithm
      , apiSession
      , apiWrappedPseudoRF
      , apiLogger
      }


-- | Attach a logger to an 'Api'; it is called after every HTTP response.
withLogger :: ApiLogger -> Api -> Api
withLogger logger api = api{apiLogger = Just logger}


{- | Build an 'ApiLogger' that appends one entry per response to a 'Handle'.

Each entry contains:

* a summary line: @TIMESTAMP METHOD URL STATUS@
* one response header per line: @Name: value@
* the raw response body
* a @---@ separator

Not safe for concurrent use from multiple threads against the same handle.
-}
fileLogger :: Handle -> ApiLogger
fileLogger h = ApiLogger $ \req resp -> do
  now <- getCurrentTime
  let scheme = if secure req then "https" else "http" :: String
      uri = scheme <> "://" <> toS (host req) <> toS (path req)
      status = statusCode (responseStatus resp)
      summary = show now <> " " <> toS (method req) <> " " <> uri <> " " <> show status
      fmtHdr (name, val) = toS (original name) <> ": " <> toS val
  hPutStrLn h summary
  mapM_ (hPutStrLn h . fmtHdr) (requestHeaders req)
  hPutStrLn h ""
  mapM_ (hPutStrLn h . fmtHdr) (responseHeaders resp)
  hPutStrLn h ""
  LBS.hPutStr h (responseBody resp)
  hPutStrLn h "\n---"


{- | Like 'fileLogger' but also logs the query string in the URL and the
request body when present.
-}
verboseLogger :: Handle -> ApiLogger
verboseLogger h = ApiLogger $ \req resp -> do
  now <- getCurrentTime
  let scheme = if secure req then "https" else "http" :: String
      qs = if BS.null (queryString req) then "" else "?" <> toS (queryString req)
      uri = scheme <> "://" <> toS (host req) <> toS (path req) <> qs
      status = statusCode (responseStatus resp)
      summary = show now <> " " <> toS (method req) <> " " <> uri <> " " <> show status
      fmtHdr (name, val) = toS (original name) <> ": " <> toS val
  hPutStrLn h summary
  mapM_ (hPutStrLn h . fmtHdr) (requestHeaders req)
  logReqBody (requestBody req)
  hPutStrLn h ""
  mapM_ (hPutStrLn h . fmtHdr) (responseHeaders resp)
  hPutStrLn h ""
  LBS.hPutStr h (responseBody resp)
  hPutStrLn h "\n---"
 where
  logReqBody (RequestBodyLBS lbs)
    | not (LBS.null lbs) = hPutStrLn h "" >> LBS.hPutStr h lbs
  logReqBody (RequestBodyBS bs)
    | not (BS.null bs) = hPutStrLn h "" >> LBS.hPutStr h (LBS.fromStrict bs)
  logReqBody _ = pure ()


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
  runReaderT loginProcess api >>= \case
    LoginAuthenticated (AuthComplete _ ad) -> pure $ Authenticated (apiSession api) ad
    LoginNeedsTwoFa (NeedsTwoFa _) -> completeTwoFactorWith readCode pickPhone api
    LoginNeedsTwoSa (TwoSaReady _ ds) -> complete2SAWith pickDevice (readCode 6) api ds
    LoginHaltCreds _ -> throwIO CredentialsMissing
    LoginHaltDir _ -> throwIO $ ArtifactDirCreationFailed (sessionTopDir (apiSession api))
    LoginHaltSrp _ -> throwIO SrpProtocolError
    LoginHaltTwoFaLocked _ -> throwIO TwoFactorLocked


instance LoginEvent (ReaderT Api IO) where
  type State (ReaderT Api IO) = LoginFSM


  initial = pure RatifyCredentials


  ratifyCreds RatifyCredentials =
    asks (GotCreds . RatifyArtifactDir . sessionCreds . apiSession)


  ratifyArtifactDir (RatifyArtifactDir creds) = do
    api <- ask
    let dir = sessionTopDir (apiSession api)
    exists <- liftIO $ doesDirectoryExist dir
    pure $
      if exists
        then DirPresent $ LoadLastSession creds
        else DirAbsent $ MkArtifactDir creds


  mkArtifactDir (MkArtifactDir creds) = do
    api <- ask
    let dir = sessionTopDir (apiSession api)
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
    let ad = parseAccountData loginReply
    liftIO $ saveLoginMsg (apiSession api) loginReply
    liftIO $ saveAccountData (apiSession api) ad
    pure $
      if
        | accountDataRequires2SA ad -> AcctLogin2SA $ NeedsTwoSa creds
        | accountDataRequires2FA ad -> AcctLogin2FA $ NeedsTwoFa creds
        | otherwise -> AcctLoginOk $ AuthComplete creds ad


  listTwoSaDevices (NeedsTwoSa creds) = do
    api <- ask
    devices <- liftIO $ listSetupDevices api
    pure $ TwoSaReady creds devices


  beginTwoFa (ReadyForTwoFa creds td) TwoFaConfig{tfcPickPhone} = do
    api <- ask
    mbPhone <- liftIO $ tfcPickPhone td
    liftIO $ case mbPhone of
      Nothing -> triggerTwoFaPush api
      Just phone -> requestSmsCode api phone
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
    api <- ask
    liftIO $ doTrustStep api
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


parseAccountData :: Value -> AccountData
parseAccountData v = fromMaybe unknownAccountData $ parseMaybe parseJSON v


-- | Fetch the 2FA options immediately after the 409 from signin/complete
fetchTrustData :: Api -> IO TrustData
fetchTrustData api = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req = withHeaders (withAcceptJson $ authHeaders api savedHdrs) (twoFaOptionsBase (apiEndpoints api))
  callApi api req >>= extractOr'


-- | POST to phone/securitycode to request an SMS code to the given phone
requestSmsCode :: Api -> TrustedPhone -> IO ()
requestSmsCode api@Api{apiEndpoints = ep} tp = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req =
        withHeaders (authHeaders api savedHdrs) $
          withJsonRequestHeaders $
            withBody (encode $ phoneTriggerBody tp) $
              toPut (extendPath (epAuth ep) "/verify/phone")
  resp <- rawRequest api req
  unless (statusCode (responseStatus resp) < 400) $
    throwIO $
      UnexpectedResponse $
        showStatusOf resp


-- | POST to phone/securitycode to verify an SMS code; returns True when accepted
verifySmsCode :: Api -> TrustedPhone -> AuthCode -> IO Bool
verifySmsCode api@Api{apiEndpoints = ep} tp code = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req =
        withHeaders (authHeaders api savedHdrs) $
          withJsonRequestHeaders $
            withBody (encode $ phoneCodeBody tp code) $
              verifySecurityCodeReq "phone" ep
  resp <- rawRequest api req
  let c = statusCode (responseStatus resp)
  if
    | c < 400 -> pure True
    | c == 400 -> pure False
    | otherwise -> throwIO $ UnexpectedResponse $ showStatusOf resp


-- | Complete a pending 2FA (auth-endpoint) challenge
completeTwoFactor :: Api -> IO AuthState
completeTwoFactor api = completeTwoFactorWith pleaseReadCode (\_ -> pure Nothing) api


-- | Like 'completeTwoFactor' with an injectable code prompt and phone selector, for testing
completeTwoFactorWith :: (Word8 -> IO AuthCode) -> (TrustData -> IO (Maybe TrustedPhone)) -> Api -> IO AuthState
completeTwoFactorWith readCode pickPhone api = do
  td <- fetchTrustData api
  let start = ReadyForTwoFa (sessionCreds (apiSession api)) td
      cfg = TwoFaConfig{tfcPickPhone = pickPhone, tfcReadCode = readCode}
  runReaderT (twoFaProcess start cfg) api >>= \case
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
  runReaderT (twoSaProcess start cfg) api >>= \case
    CompletionAuthenticated (AuthComplete _ ad) -> pure $ Authenticated (apiSession api) ad
    CompletionNeedsTwoFa _ -> throwIO TwoFactorStillRequired
    CompletionNeedsTwoSa (TwoSaReady _ ds) -> pure $ Requires2SA (apiSession api) ds
    CompletionTwoFaLocked _ -> throwIO TwoFactorLocked


-- | Make a session request and obtain the raw byte results
rawRequest :: Api -> Request -> IO (Response LBS.ByteString)
rawRequest = rawRequest' True


-- | Make a session request and obtain the raw byte results
rawRequest' :: Bool -> Api -> Request -> IO (Response LBS.ByteString)
rawRequest' mayRetry api req = do
  let Api{apiManager = mgr, apiSession = s, apiLogger = mbLogger} = api
      jarPath = cookiePath (sessionTopDir s) (sessionCreds s)
  resp <- usingCookiesFromFile jarPath req $ flip httpLbs mgr
  updateSessionSavedHeaders s $ updateSavedHeaders $ responseHeaders resp
  mapM_ (\(ApiLogger logFn) -> logFn req resp) mbLogger
  if mayRetry && needsRetry (statusCode (responseStatus resp))
    then rawRequest' False api req
    else pure resp


{- | Make a session request to obtain a JSON payload

call api with request, obtain response
save the sessionData from the response headers
save any cookies from the response headers
if the response is JSON, parse it, and see if it parses as an ApiError
if JSON parsing fails, log to stderr
if it parses as an ApiError, indicate that

if the response is not JSON, use 'rawRequest' instead
-}
callApi
  :: (FromJSON a) => Api -> Request -> IO (Response (ApiResponse a))
callApi api req = do
  let isJsonType ct = "application/json" `isPrefixOf` ct || "text/json" `isPrefixOf` ct
  raw <- rawRequest api req
  let code = statusCode (responseStatus raw)
      theType = lookup hContentType $ responseHeaders raw
      isJson = maybe False isJsonType theType
  when (code >= 400 && LBS.null (responseBody raw)) $
    throwIO $
      UnexpectedResponse $
        showStatusOf raw
  unless (code >= 400 || isJson) $
    throwIO $
      UnexpectedResponse $
        "response was not JSON: " <> toS (show theType)
  mapM asJson raw


-- confirm the content-type of the response before attempting to parse
-- if it's wrong, throw  InvalidContentType
-- try to parse, if that fails, throw WrongDataType
asJson :: (FromJSON a) => LBS.ByteString -> IO a
asJson resp = case eitherDecode resp of
  Left _err -> throwIO $ UnexpectedResponse "did not decode JSON response correctly"
  Right x -> pure x


extractOr' :: Response (ApiResponse a) -> IO a
extractOr' r | statusCode (responseStatus r) >= 400 = throwIO $ UnexpectedResponse $ showStatusOf r
extractOr' r = extractOr $ responseBody r


showStatusOf :: Response a -> Text
showStatusOf resp =
  let showResponse' x s | x >= 500 = "server error:" <> Text.pack (show s)
      showResponse' x s | x >= 400 = "bad request:" <> Text.pack (show s)
      showResponse' _x s = "ok:" <> Text.pack (show s)
      theStatus = responseStatus resp
      theCode = statusCode theStatus
   in showResponse' theCode theStatus


authHeaders :: Api -> SavedHeaders -> RequestHeaders
authHeaders api savedHdrs =
  let Api{apiSession = session, apiEndpoints = ep} = api
      Session{sessionClientId = cid} = session
      headerOf name x = (name, toS x)
      maybeHeaderOf name = fmap (headerOf name)
      cidHeader = [(hClientId, toS cid)]
      sdHeaders =
        catMaybes
          [ maybeHeaderOf hCounter $ shCounter savedHdrs
          , maybeHeaderOf hSessionId $ shSessionId savedHdrs
          ]
   in withAppleOauthHeaders $ homeHeaders ep <> sdHeaders <> cidHeader


requiredHeaders :: SavedHeaders -> RequestHeaders
requiredHeaders savedHdrs =
  let headerOf name x = (name, toS x)
      maybeHeaderOf name = fmap (headerOf name)
      sdHeaders =
        catMaybes
          [ maybeHeaderOf hCounter $ shCounter savedHdrs
          , maybeHeaderOf hSessionId $ shSessionId savedHdrs
          ]
   in withAcceptJson . withICloudWidgetKey $ sdHeaders


maybeValue :: (a -> Value) -> Maybe a -> Value
maybeValue = maybe Null


asObject :: [(Key, Value)] -> Value
asObject = Object . fromList


mkJsonRequest :: (a -> Request) -> (b -> Value) -> a -> b -> Request
mkJsonRequest mkBase mkBody baseSrc bodySrc =
  withJsonRequestHeaders . withBody (encode $ mkBody bodySrc) $ mkBase baseSrc


withJsonRequestHeaders :: Request -> Request
withJsonRequestHeaders = withHeaders [(hAccept, "application/json"), (hContentType, "application/json")]


callHandlingResponse
  :: (FromJSON a)
  => (Endpoints -> b -> Request)
  -> (Request -> Request)
  -> Api
  -> b
  -> IO a
callHandlingResponse mkReq modReq api@Api{apiEndpoints} x =
  callApi api (modReq $ mkReq apiEndpoints x) >>= extractOr'


-- | @HeaderName@ used to represent API session data
hClientId :: HeaderName
hClientId = mk "X-Apple-OAuth-State"


data SigninInitReply = SigninInitReply
  { sirTag :: !Text
  , sirProtocol :: !PasswordProtocol
  , sirPublicBytes :: !ByteString
  , sirIterations :: !Word64
  , sirSalt :: !ByteString
  }
  deriving (Eq, Show)


instance FromJSON SigninInitReply where
  parseJSON = withObject "SigninInitReply" parseSigninInitReply


parseSigninInitReply :: Object -> Parser SigninInitReply
parseSigninInitReply o =
  let tag = o .: "c"
      iterations = o .: "iteration"
      protocol = o .: "protocol"
      publicBytes = o .: "b" >>= parseBase64Bytes
      salt = o .: "salt" >>= parseBase64Bytes
      parseBase64Bytes s = case decodeBase64Untyped (encodeUtf8 s) of
        Left err -> fail $ Text.unpack err
        Right b -> pure b
   in SigninInitReply
        <$> tag
        <*> protocol
        <*> publicBytes
        <*> iterations
        <*> salt


signinInit :: Api -> FromClient -> IO SigninInitReply
signinInit api other = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  callHandlingResponse signinInitReq (withHeaders (authHeaders api savedHdrs)) api other


runSigninInit :: Api -> FromClient -> IO (FromServer, KeyDeriver)
runSigninInit api fc = do
  r <- signinInit api fc
  let fromServer =
        FromServer
          { fsPublicBytes = sirPublicBytes r
          , fsSalt = sirSalt r
          , fsPrimeGroup = apiGroup api
          , fsKnownAlgorithm = apiHashAlgorithm api
          }
      keyDeriver =
        KeyDeriver
          { kdTag = sirTag r
          , kdIterations = sirIterations r
          , kdWrappedF = apiWrappedPseudoRF api
          , kdProtocol = sirProtocol r
          }
  pure (fromServer, keyDeriver)


signinInitReq :: Endpoints -> FromClient -> Request
signinInitReq = mkJsonRequest signinInitBase signinInitValue


signinInitValue :: FromClient -> Value
signinInitValue fc =
  let a = extractBase64 $ encodeBase64 $ fcPublicBytes fc
   in asObject
        [ ("a", String a)
        , ("accountName", String (fcUser fc))
        , ("protocols", Array ["s2k", "s2k_fo"])
        ]


data SigninCompletion = SigninCompletion
  { siTag :: !Text
  , siAccountName :: !Text
  , siSavedHeaders :: !SavedHeaders
  , siResults :: !Results
  }


runSigninComplete :: Api -> KeyDeriver -> Results -> IO ()
runSigninComplete api@Api{apiSession = session} kd results = do
  siSavedHeaders <- loadSavedHeaders session
  let siAccountName = credAccountName $ sessionCreds session
      completion =
        SigninCompletion
          { siTag = kdTag kd
          , siAccountName
          , siResults = results
          , siSavedHeaders
          }
  signinComplete api completion


signinComplete :: Api -> SigninCompletion -> IO ()
signinComplete api sc = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req = withHeaders (authHeaders api savedHdrs) $ signinCompleteReq (apiEndpoints api) sc
  resp <- callApi api req :: IO (Response (ApiResponse ()))
  handleSigninComplete resp


signinCompleteReq :: Endpoints -> SigninCompletion -> Request
signinCompleteReq = mkJsonRequest signinCompleteBase signinCompleteValue


signinCompleteValue :: SigninCompletion -> Value
signinCompleteValue sc =
  let Results{rClientProof, rServerProof} = siResults sc
      toBase64 = extractBase64 . encodeBase64
      singleElem x = Array [String x]
      maybeArray = maybe (Array []) singleElem
   in asObject
        [ ("m1", String (toBase64 rClientProof))
        , ("m2", String (toBase64 rServerProof))
        , ("trustTokens", maybeArray (shTrustToken (siSavedHeaders sc)))
        , ("rememberMe", Bool True)
        , ("accountName", String (siAccountName sc))
        , ("c", String (siTag sc))
        ]


handleSigninComplete :: Response (ApiResponse ()) -> IO ()
handleSigninComplete resp = do
  let code = statusCode $ responseStatus resp
  if
    | code == 401 -> throwIO InvalidCredentials
    | code == 403 -> throwIO AccountLocked
    | code == 412 -> throwIO PrivacyAgreementRequired
    | code == 409 -> pure () -- 2FA required; accountLogin will detect it
    | code >= 400 -> throwIO $ UnexpectedResponse $ showStatusOf resp
    | otherwise -> pure ()


validate :: Api -> IO Bool
validate api@Api{apiEndpoints} = do
  resp <- rawRequest api (validateReq apiEndpoints)
  let code = statusCode (responseStatus resp)
  if
    | code == 401 -> pure False
    | code >= 400 -> throwIO $ UnexpectedResponse $ showStatusOf resp
    | otherwise -> pure True


validateReq :: Endpoints -> Request
validateReq = withJsonRequestHeaders . withBody (encode Null) . validateBase


accountLogin :: Api -> IO Value
accountLogin api@Api{apiEndpoints = ep} = do
  savedHdrs <- loadSavedHeaders $ apiSession api
  let hdrs = homeHeaders ep
  callHandlingResponse accountLoginReq (withHeaders hdrs) api savedHdrs


accountLoginReq :: Endpoints -> SavedHeaders -> Request
accountLoginReq = mkJsonRequest accountLoginBase accountLoginValue


triggerTwoFaPush :: Api -> IO ()
triggerTwoFaPush api@Api{apiEndpoints = ep} = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req =
        withHeaders
          (withAcceptJson $ authHeaders api savedHdrs)
          (toPut (extendPath (epAuth ep) "/verify/trusteddevice/securitycode"))
  -- The server sometimes returns a non-2xx here when a push is already in flight;
  -- the device still shows the notification, so errors are safe to ignore.
  void (rawRequest api req) `catch` \(_ :: IOException) -> pure ()


doTrustStep :: Api -> IO ()
doTrustStep api@Api{apiEndpoints = ep} = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let req = withHeaders (withAcceptJson $ authHeaders api savedHdrs) (twoSvTrust ep)
  resp <- rawRequest api req
  unless (statusCode (responseStatus resp) < 400) $
    throwIO $
      UnexpectedResponse $
        showStatusOf resp


verifyTwoFaCode :: Api -> AuthCode -> IO Bool
verifyTwoFaCode api@Api{apiEndpoints = ep} code = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  let body = encode $ Object [("securityCode", Object [("code", String code)])]
      req =
        withHeaders (authHeaders api savedHdrs) $
          withJsonRequestHeaders $
            withBody body $
              verifySecurityCodeReq "trusteddevice" ep
  resp <- rawRequest api req
  let c = statusCode (responseStatus resp)
  if
    | c < 400 -> pure True
    | c == 400 -> pure False
    | otherwise -> throwIO $ UnexpectedResponse $ showStatusOf resp


callRequiredHeaders :: (FromJSON a) => Api -> Request -> IO a
callRequiredHeaders api@Api{apiSession = s} req = do
  savedHdrs <- loadSavedHeaders s
  callApi api (withHeaders (requiredHeaders savedHdrs) req) >>= extractOr'


accountLoginValue :: SavedHeaders -> Value
accountLoginValue hs =
  asObject
    [ ("accountCountryCode", maybeValue String (shCountry hs))
    , ("dsWebAuthToken", maybeValue String (shSessionToken hs))
    , ("trustToken", String $ fromMaybe "" $ shTrustToken hs)
    , ("extended_login", Bool True)
    ]


newtype ListDevicesReply = ListDevicesReply {ldrDevices :: [Setup2SADevice]}


instance FromJSON ListDevicesReply where
  parseJSON = withObject "ListDevicesReply" $ \o -> ListDevicesReply <$> o .: "devices"


listSetupDevices :: Api -> IO [Setup2SADevice]
listSetupDevices api@Api{apiEndpoints = ep} =
  ldrDevices <$> callRequiredHeaders api (listDevices ep)


sendSetupVerification :: Api -> Setup2SADevice -> IO ()
sendSetupVerification api@Api{apiSession = s, apiEndpoints = ep} device = do
  savedHdrs <- loadSavedHeaders s
  let req =
        withHeaders (requiredHeaders savedHdrs) $
          withJsonRequestHeaders $
            withBody (encode device) $
              sendVerification ep
  resp <- rawRequest api req
  unless (statusCode (responseStatus resp) < 400) $ throwIO $ UnexpectedResponse $ showStatusOf resp


validateSetupVerification :: Api -> Setup2SADevice -> AuthCode -> IO Bool
validateSetupVerification api@Api{apiSession = s, apiEndpoints = ep} device code = do
  savedHdrs <- loadSavedHeaders s
  let req =
        withHeaders (requiredHeaders savedHdrs) $
          withJsonRequestHeaders $
            withBody (encode $ validateSetupBody device code) $
              validateVerification ep
  resp <- rawRequest api req
  pure $ statusCode (responseStatus resp) < 400


{- | The code sent to a user device that the user must enter to confirm
authenticity
-}
type AuthCode = Text
