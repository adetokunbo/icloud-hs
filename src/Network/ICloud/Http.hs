{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Network.ICloud.Http
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Network.ICloud.Http
  ( -- * functions
    mkApi
  , mkApiWith
  , login
  , loginWith
  , completeTwoFactor
  , completeTwoFactorWith
  , complete2SA
  , complete2SAWith
  , validateSetupBody

    -- * types
  , Api
  , AuthState (..)

    -- * types
  , PasswordProtocol (..)

    -- * classes
  , AsVerifyRequest (..)

    -- * errors
  , AuthError (..)
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless)
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
  , ToJSON (..)
  , eitherDecode
  , encode
  , withObject
  , (.:)
  )
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Types (Parser, Value (..), parseMaybe)
import Data.Base64.Types (extractBase64)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64Untyped, encodeBase64)
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , Response (..)
  , httpLbs
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
  ( HeaderName
  , RequestHeaders
  , Status (..)
  , hContentType
  )
import Network.ICloud.Http.Endpoints
  ( Endpoints (..)
  , Realm
  , accountLoginBase
  , extendPath
  , homeHeaders
  , listDevices
  , realmEndpoints
  , sendVerification
  , signinCompleteBase
  , signinInitBase
  , toPut
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
import Network.ICloud.Http.Errors
  ( ApiResponse
  , AuthError (..)
  , ExtractOr (..)
  , extractOrRetry
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
  , AtEnd (..)
  , BeforeEnd (..)
  , LoginEvent (..)
  , LoginFSM (..)
  , loginProcess
  , twoFaProcess
  , twoSaProcess
  )
import Network.ICloud.PBKDF2 (FancyPseudoRandomF, wrapIO)
import Network.ICloud.Session
  ( AccountData (..)
  , Credentials (..)
  , KeyDeriver (..)
  , PasswordProtocol (..)
  , SavedHeaders (..)
  , Session (..)
  , SrpContext (..)
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
import qualified Network.ICloud.Session as Session
import Network.ICloud.Trust
  ( Setup2SADevice (..)
  , TrustData
  , TrustedDevice (..)
  , TrustedPhone (..)
  , pleaseReadCode
  , selectSetupDevice
  , withSelectedPhoneOrDevice
  )
import Web.Cookie.Jar (usingCookiesFromFile)


-- | Combines datatypes used whenever the http API is accessed
data Api = Api
  { apiManager :: !Manager
  , apiSession :: !Session
  , apiEndpoints :: !Endpoints
  , apiHashAlgorithm :: !KnownAlgorithm
  , apiWrappedPseudoRF :: !FancyPseudoRandomF
  , apiGroup :: !PrimeGroup
  }


-- | Constructor of @Api@
mkApi :: Realm -> IO Api
mkApi realm = do
  let apiHashAlgorithm = SHA256
      apiGroup = G2048
      apiEndpoints = realmEndpoints realm
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
      }


-- | Constructor of @Api@ with a pre-built @Manager@ and @Endpoints@, for testing
mkApiWith :: Session -> Endpoints -> Manager -> IO Api
mkApiWith apiSession apiEndpoints apiManager = do
  let apiHashAlgorithm = SHA256
      apiGroup = G2048
  apiWrappedPseudoRF <- wrapIO SHA256.hmac $ digestSize apiHashAlgorithm
  pure
    Api
      { apiGroup
      , apiEndpoints
      , apiManager
      , apiHashAlgorithm
      , apiSession
      , apiWrappedPseudoRF
      }


-- | The result of a login attempt
data AuthState
  = Authenticated Session AccountData
  | Requires2FA Session TrustData
  | Requires2SA Session [Setup2SADevice]


instance Show AuthState where
  show (Authenticated _ ad) = "Authenticated <session> " ++ show ad
  show (Requires2FA _ td) = "Requires2FA <session> " ++ show td
  show (Requires2SA _ ds) = "Requires2SA <session> " ++ show ds


-- | Logs into ICloud, completing any 2FA or 2SA challenge automatically
login :: Api -> IO AuthState
login = loginWith pleaseReadCode selectSetupDevice


-- | Like 'login' with injectable code prompt and device selector, for testing
loginWith
  :: IO AuthCode
  -> ([Setup2SADevice] -> IO Setup2SADevice)
  -> Api
  -> IO AuthState
loginWith readCode pickDevice api = do
  runReaderT (loginProcess >>= end) api >>= \case
    Normal _ ad -> pure $ Authenticated (apiSession api) ad
    Needs2FA _ td -> completeTwoFactorWith readCode api td
    Needs2SA _ ds -> complete2SAWith pickDevice readCode api ds
    Halted -> throwIO $ UnexpectedResponse "login halted"


instance LoginEvent (ReaderT Api IO) where
  type State (ReaderT Api IO) = LoginFSM


  initial = pure RatifyCredentials


  ratifyCreds RatifyCredentials =
    asks (GotCreds . RatifyArtifactDir . sessionCreds . apiSession)


  ratifyArtifactDir (RatifyArtifactDir creds) =
    pure $ DirPresent $ LoadLastSession creds


  mkArtifactDir (MkArtifactDir creds) =
    pure $ DirMade $ LoadLastSession creds


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


  mkClientId (MakeClientId creds savedHdrs) =
    pure $ ReadyToAuth creds savedHdrs


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
        mbResults = calcResults kd fc fs
    result <- liftIO $ runSigninComplete api kd mbResults
    pure $ case result of
      Left td -> SrpComplete2FA $ NeedsTwoFa creds td
      Right () -> SrpCompleteOk $ IncreaseTrust creds


  increaseTrust (IncreaseTrust creds) = do
    api <- ask
    liftIO (callRequiredHeaders api (twoSvTrust (apiEndpoints api)) :: IO ())
    pure $ DoAccountLogin creds


  acctLogin (DoAccountLogin creds) = do
    api <- ask
    loginReply <- liftIO $ accountLogin api
    let ad = parseAccountData loginReply
    liftIO $ saveLoginMsg (apiSession api) loginReply
    liftIO $ saveAccountData (apiSession api) ad
    pure $
      if accountDataRequires2SA ad
        then AcctLogin2SA $ NeedsTwoSa creds
        else AcctLoginOk $ AuthComplete creds ad


  listTwoSaDevices (NeedsTwoSa creds) = do
    api <- ask
    devices <- liftIO $ listSetupDevices api
    pure $ TwoSaReady creds devices


  beginTwoFa (ReadyForTwoFa creds td readCode) = do
    api <- ask
    liftIO $ withSelectedPhoneOrDevice (askForTwoFactorCode api) (askForTwoStepCode api) td
    pure $ TwoFaVerifying creds td readCode


  verifyTwoFa (TwoFaVerifying creds td readCode) = do
    api <- ask
    code <- liftIO readCode
    let doVerify =
          withSelectedPhoneOrDevice
            (\tp -> verifyCodeOrRetry api tp code)
            (\td' -> verifyCodeOrRetry api td' code)
            td
    mbResult <- liftIO (doVerify :: IO (Maybe ()))
    pure $ case mbResult of
      Nothing -> TwoFaRetry $ ReadyForTwoFa creds td readCode
      Just () -> TwoFaOk $ DoAccountLogin creds


  beginTwoSa (ReadyForTwoSa creds devices pickDevice readCode) = do
    api <- ask
    device <- liftIO $ pickDevice devices
    liftIO $ sendSetupVerification api device
    pure $ TwoSaVerifying creds device devices pickDevice readCode


  verifyTwoSa (TwoSaVerifying creds device devices pickDevice readCode) = do
    api <- ask
    code <- liftIO readCode
    ok <- liftIO $ validateSetupVerification api device code
    pure $
      if ok
        then TwoSaOk $ DoAccountLogin creds
        else TwoSaRetry $ ReadyForTwoSa creds devices pickDevice readCode


  end (EndedAuthenticated (AuthComplete creds ad)) = pure $ Normal creds ad
  end (EndedNeedsTwoFa (NeedsTwoFa creds td)) = pure $ Needs2FA creds td
  end (EndedNeedsTwoSa (TwoSaReady creds ds)) = pure $ Needs2SA creds ds
  end (EndedAfterCredentials _) = pure Halted
  end (EndedAfterMkArtifactDir _) = pure Halted
  end (EndedHaltInvalidSrp _) = pure Halted


parseAccountData :: Value -> AccountData
parseAccountData v = fromMaybe unknownAccountData $ parseMaybe parseJSON v


-- | Complete a 2FA (auth-endpoint) challenge after a @Requires2FA@ result
completeTwoFactor :: Api -> TrustData -> IO AuthState
completeTwoFactor = completeTwoFactorWith pleaseReadCode


-- | Like 'completeTwoFactor' with an injectable code prompt, for testing
completeTwoFactorWith :: IO AuthCode -> Api -> TrustData -> IO AuthState
completeTwoFactorWith readCode api td = do
  let start = ReadyForTwoFa (sessionCreds (apiSession api)) td readCode
  runReaderT (twoFaProcess start >>= end) api >>= \case
    Normal _ ad -> pure $ Authenticated (apiSession api) ad
    Needs2FA _ td' -> pure $ Requires2FA (apiSession api) td'
    Needs2SA _ ds -> pure $ Requires2SA (apiSession api) ds
    Halted -> throwIO $ UnexpectedResponse "completeTwoFactor halted"


-- | Complete a 2SA (setup-endpoint) challenge after a @Requires2SA@ result
complete2SA :: Api -> [Setup2SADevice] -> IO AuthState
complete2SA = complete2SAWith selectSetupDevice pleaseReadCode


-- | Like 'complete2SA' with injectable device selector and code prompt, for testing
complete2SAWith
  :: ([Setup2SADevice] -> IO Setup2SADevice)
  -> IO AuthCode
  -> Api
  -> [Setup2SADevice]
  -> IO AuthState
complete2SAWith pickDevice readCode api devices = do
  let start = ReadyForTwoSa (sessionCreds (apiSession api)) devices pickDevice readCode
  runReaderT (twoSaProcess start >>= end) api >>= \case
    Normal _ ad -> pure $ Authenticated (apiSession api) ad
    Needs2FA _ td -> pure $ Requires2FA (apiSession api) td
    Needs2SA _ ds -> pure $ Requires2SA (apiSession api) ds
    Halted -> throwIO $ UnexpectedResponse "complete2SA halted"


-- | Make a session request and obtain the raw byte results
rawRequest :: Api -> Request -> IO (Response LBS.ByteString)
rawRequest = rawRequest' True


-- | Make a session request and obtain the raw byte results
rawRequest' :: Bool -> Api -> Request -> IO (Response LBS.ByteString)
rawRequest' mayRetry api req = do
  let Api{apiManager = mgr, apiSession = s} = api
      jarPath = cookiePath (sessionTopDir s) (sessionCreds s)
  resp <- usingCookiesFromFile jarPath req $ flip httpLbs mgr
  updateSessionSavedHeaders s $ updateSavedHeaders $ responseHeaders resp
  if mayRetry && needsRetry resp
    then rawRequest' False api req
    else pure resp


needsRetry :: Response a -> Bool
needsRetry resp =
  let status = statusCode $ responseStatus resp
   in status == 421 || status == 450 || status == 500


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
  let isJsonType "application/json" = True
      isJsonType "text/json" = True
      isJsonType _other = False
  raw <- rawRequest api req
  let theType = lookup hContentType $ responseHeaders raw
      isJson = maybe False isJsonType theType
  unless isJson $ throwIO $ UnexpectedResponse $ "response was not JSON: " <> toS (show theType)
  mapM asJson raw


callSEReply
  :: (FromJSON a) => Api -> Request -> IO (Maybe a)
callSEReply api req =
  let
    extractOrRetry' r | statusCode (responseStatus r) >= 400 = throwIO $ UnexpectedResponse $ showStatusOf r
    extractOrRetry' r = extractOrRetry $ responseBody r
   in
    rawRequest api req >>= mapM asJson >>= extractOrRetry'


-- confirm the content-type of the response before attempting to parse
-- if it's wrong, throw  InvalidContentType
-- try to parse, if that fails, throw WrongDataType
asJson :: (FromJSON a) => LBS.ByteString -> IO a
asJson resp = case eitherDecode resp of
  Left _err -> throwIO $ UnexpectedResponse "did not decode JSON response correctly"
  Right x -> pure x


extractOr' :: (ExtractOr a b) => Response (b a) -> IO a
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
  withBody (encode $ mkBody bodySrc) $ mkBase baseSrc


callHandlingResponse
  :: (FromJSON a)
  => (Endpoints -> b -> Request)
  -> (Request -> Request)
  -> (Response (ApiResponse a) -> IO a)
  -> Api
  -> b
  -> IO a
callHandlingResponse mkReq modReq handleResponse api@Api{apiEndpoints} x =
  callApi api (modReq $ mkReq apiEndpoints x) >>= handleResponse


-- | @HeaderName@ used to represent API session data
hSessionId
  , hCounter
  , hClientId
    :: HeaderName
hSessionId = mk "X-Apple-ID-Session-Id"
hCounter = mk "scnt"
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
  callHandlingResponse signinInitReq (withHeaders (authHeaders api savedHdrs)) extractOr' api other


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


runSigninComplete :: Api -> KeyDeriver -> Maybe Results -> IO (Either TrustData ())
runSigninComplete api@Api{apiSession = session} kd mbResults = do
  siSavedHeaders <- loadSavedHeaders session
  let siAccountName = credAccountName $ sessionCreds session
      completion siResults =
        SigninCompletion
          { siTag = kdTag kd
          , siAccountName
          , siResults
          , siSavedHeaders
          }
      onFail = throwIO $ UnexpectedResponse "the server public value was invalid"
  maybe onFail (signinComplete api . completion) mbResults


signinComplete :: Api -> SigninCompletion -> IO (Either TrustData ())
signinComplete api sc = do
  resp <- callApi api (signinCompleteReq (apiEndpoints api) sc) :: IO (Response (ApiResponse ()))
  handleSigninComplete api resp


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


handleSigninComplete :: Api -> Response (ApiResponse ()) -> IO (Either TrustData ())
handleSigninComplete api resp = do
  let code = statusCode $ responseStatus resp
      body = responseBody resp
  if
    | code == 401 -> throwIO InvalidCredentials
    | code == 403 -> throwIO AccountLocked
    | code == 412 -> throwIO PrivacyAgreementRequired
    | code == 409 -> Left <$> chooseTrustType api
    | code >= 400 -> throwIO $ UnexpectedResponse $ showStatusOf resp
    | otherwise -> Right <$> extractOr body


verifyCodeOrRetry :: (FromJSON a, AsVerifyRequest b) => Api -> b -> AuthCode -> IO (Maybe a)
verifyCodeOrRetry api x code =
  let req' = verifySecurityCodeReq (verifyCodeType x) $ apiEndpoints api
      req = withBody (encode $ asVerifyRequest x code) req'
   in callSEReply api req


validate :: Api -> IO Bool
validate api@Api{apiEndpoints} = do
  resp <- rawRequest api (validateReq apiEndpoints)
  let code = statusCode (responseStatus resp)
  if
    | code == 401 -> pure False
    | code >= 400 -> throwIO $ UnexpectedResponse $ showStatusOf resp
    | otherwise -> pure True


validateReq :: Endpoints -> Request
validateReq = withBody (encode Null) . validateBase


accountLogin :: Api -> IO Value
accountLogin api = do
  savedHdrs <- loadSavedHeaders $ apiSession api
  callHandlingResponse accountLoginReq id extractOr' api savedHdrs


accountLoginReq :: Endpoints -> SavedHeaders -> Request
accountLoginReq = mkJsonRequest accountLoginBase accountLoginValue


chooseTrustType :: Api -> IO TrustData
chooseTrustType api@Api{apiEndpoints = ep} = callRequiredHeaders api (epAuth ep)


callRequiredHeaders :: (FromJSON a) => Api -> Request -> IO a
callRequiredHeaders api@Api{apiSession = s} req = do
  savedHdrs <- loadSavedHeaders s
  callApi api (withHeaders (requiredHeaders savedHdrs) req) >>= extractOr'


accountLoginValue :: SavedHeaders -> Value
accountLoginValue hs =
  asObject
    [ ("accountCountryCode", maybeValue String (shCountry hs))
    , ("dsWebAuthToken", maybeValue String (shSessionToken hs))
    , ("trustToken", maybeValue String (shTrustToken hs))
    , ("extended_login", Bool True)
    ]


askForTwoStepCode :: Api -> TrustedDevice -> IO ()
askForTwoStepCode api@Api{apiEndpoints = ep} td =
  let pathTail = toS $ "/" <> tdId td <> "/securitycode"
      mkReqBase = (`extendPath` "/verify/device") . toPut . epAuth
      mkReq = (`extendPath` pathTail) . mkReqBase
   in callRequiredHeaders api (mkReq ep)


askForTwoFactorCode :: Api -> TrustedPhone -> IO ()
askForTwoFactorCode api tp = do
  let mode = fromMaybe "sms" $ tpnPushMode tp
      mkReq =
        (`extendPath` "/verify/phone")
          . toPut
          . withHeaders [(hContentType, "application/json")]
          . epAuth
      value =
        Object
          [ ("mode", String mode)
          , ("phoneNumber", Object [("id", toJSON (tpnId tp))])
          ]
      req = withBody (encode value) $ mkReq $ apiEndpoints api
  callRequiredHeaders api req


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
          withBody (encode device) $
            sendVerification ep
  resp <- rawRequest api req
  unless (statusCode (responseStatus resp) < 400) $ throwIO $ UnexpectedResponse $ showStatusOf resp


validateSetupBody :: Setup2SADevice -> AuthCode -> Value
validateSetupBody (Setup2SADevice fields) code =
  Object $ fields <> fromList [("verificationCode", String code), ("trustBrowser", Bool True)]


validateSetupVerification :: Api -> Setup2SADevice -> AuthCode -> IO Bool
validateSetupVerification api@Api{apiSession = s, apiEndpoints = ep} device code = do
  savedHdrs <- loadSavedHeaders s
  let req =
        withHeaders (requiredHeaders savedHdrs) $
          withBody (encode $ validateSetupBody device code) $
            validateVerification ep
  resp <- rawRequest api req
  pure $ statusCode (responseStatus resp) < 400


{- | The code sent to a user device that the user must enter to confirm
authenticity
-}
type AuthCode = Text


class AsVerifyRequest a where
  asVerifyRequest :: a -> AuthCode -> Value
  verifyCodeType :: a -> Text


instance AsVerifyRequest TrustedPhone where
  verifyCodeType _ = "phone"
  asVerifyRequest tpn code =
    Object
      [ ("securityCode", String code)
      , ("mode", String "sms")
      , ("phoneNumber", Object [("id", toJSON (tpnId tpn))])
      ]


instance AsVerifyRequest TrustedDevice where
  verifyCodeType _ = "trusteddevice"
  asVerifyRequest td code =
    Object
      [ ("securityCode", String code)
      , ("mode", String "sms")
      , ("phoneNumber", String (tdId td))
      ]
