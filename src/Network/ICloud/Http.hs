{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.ICloud.Http
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Network.ICloud.Http (
  -- * data types
  ApiError (..),
  ApiResponse (..),
  Endpoints (..),
  Realm (..),
  realmEndpoints,
  signinInitBase,

  -- * functions
  accountLogin,
  mkSavedHeaders,
  mkApi,
  runApiSrpAuth,

  -- * HTTP header names
  hCounter,
  hCountry,
  hSessionId,
  hSessionToken,
  hTrustToken,
  hOrigin,
) where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (unless)
import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.SRP (
  FromClient (..),
  FromServer (..),
  KnownAlgorithm,
  PrimeGroup,
  Results (..),
  XCalculator (..),
  hashMany,
  hashText,
  mkFromClient,
 )
import Data.Aeson (
  FromJSON (..),
  Key,
  Object,
  eitherDecode,
  eitherDecodeFileStrict,
  encode,
  encodeFile,
  withObject,
  withText,
  (.:),
 )
import Data.Aeson.KeyMap (fromList, member)
import Data.Aeson.Types (Parser, Value (..), (.:?))
import Data.Attoparsec.Cookie (readJar, writeNetscapeJar)
import Data.Base64.Types (extractBase64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Base64 (decodeBase64Untyped, encodeBase64)
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.Maybe (catMaybes)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.HTTP.Client (
  Manager,
  Request (..),
  RequestBody (..),
  Response (..),
  createCookieJar,
  defaultRequest,
  httpLbs,
  updateCookieJar,
 )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (
  Header,
  HeaderName,
  RequestHeaders,
  Status (..),
  hContentType,
  hReferer,
  methodGet,
  methodPost,
 )
import Network.ICloud.Auth (
  Credentials (..),
  SavedHeaders (..),
  Session (..),
  cookiePath,
  loadSession,
  runSrpAuth,
  savedHeadersPath,
 )
import Network.ICloud.KDF (FancyPseudoRandomF, calcPBKDF2, wrapIO)
import System.Directory (createDirectoryIfMissing, doesFileExist)


-- | Combines datatypes used whenever the http API is accessed
data Api = Api
  { apiManager :: !Manager
  , apiSession :: !Session
  , apiEndpoints :: !Endpoints
  , apiHashAlgorithm :: !KnownAlgorithm
  , apiWrappedPseudoRF :: !FancyPseudoRandomF
  , apiGroup :: !PrimeGroup
  }


mkApi :: PrimeGroup -> KnownAlgorithm -> Realm -> IO Api
mkApi apiGroup apiHashAlgorithm realm = do
  apiManager <- newTlsManager
  apiSession <- loadSession
  apiWrappedPseudoRF <- wrapIO SHA256.hmac 32
  let apiEndpoints = realmEndpoints realm
  pure
    Api
      { apiGroup
      , apiEndpoints
      , apiManager
      , apiHashAlgorithm
      , apiSession
      , apiWrappedPseudoRF
      }


runApiSrpAuth :: (FromJSON a) => Api -> IO a
runApiSrpAuth api@Api {apiSession} = do
  let mkClientSide = mkFromClient user password $ apiGroup api
      stepOne = runSigninInit api
      stepTwo = runSigninComplete api
      creds = sessionCreds apiSession
      Credentials {credAccountName = user, credPassword = password} = creds
  runSrpAuth mkClientSide stepOne stepTwo


-- | Make a session request and obtain the raw byte results
rawRequest :: Api -> Request -> IO (Response LBS.ByteString)
rawRequest = rawRequest' True


-- | Make a session request and obtain the raw byte results
rawRequest' :: Bool -> Api -> Request -> IO (Response LBS.ByteString)
rawRequest' mayRetry api req = do
  let Api {apiManager = mgr, apiSession = s} = api
  resp <- httpLbs req mgr
  resp' <- updateCookieJarOf s resp req
  updateSavedHeadersOf s $ responseHeaders resp'
  if mayRetry && needsRetry resp'
    then rawRequest' False api req
    else pure resp'


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
jsonSessionRequest ::
  (FromJSON a) => Api -> Request -> IO (Response (ApiResponse a))
jsonSessionRequest api req = do
  let isJsonType "application/json" = True
      isJsonType "text/json" = True
      isJsonType _other = False
  raw <- rawRequest api req
  let theType = lookup hContentType $ responseHeaders raw
      isJson = maybe False isJsonType theType
  unless isJson $ fail $ "response was not JSON: " ++ show theType
  mapM asJson raw


-- confirm the content-type of the response before attempting to parse
-- if it's wrong, throw  InvalidContentType
-- try to parse, if that fails, throw WrongDataType
asJson :: (FromJSON a) => LBS.ByteString -> IO a
asJson resp = case eitherDecode resp of
  Left _err -> fail "did not decode JSON response correctly"
  Right x -> pure x


failIfError :: Response (ApiResponse a) -> IO a
failIfError resp = do
  if
    | statusCode (responseStatus resp) >= 400 -> fail $ showStatusOf resp
    | Failed x <- responseBody resp -> fail $ Text.unpack $ aeReason x
    | Succeeded x <- responseBody resp -> pure x


showStatusOf :: Response a -> String
showStatusOf resp =
  let showResponse' x s | x >= 400 = "bad request:" ++ show s
      showResponse' x s | x >= 500 = "server error:" ++ show s
      showResponse' _x s = "ok:" ++ show s
      theStatus = responseStatus resp
      theCode = statusCode theStatus
   in showResponse' theCode theStatus


{--| Represents an API response that may succeed or fail with 'ApiError' -}
data ApiResponse a = Failed !ApiError | Succeeded !a
  deriving (Eq, Show)


instance (FromJSON a) => FromJSON (ApiResponse a) where
  parseJSON v = (Failed <$> parseJSON v) <|> (Succeeded <$> parseJSON v)


{--| Represents an API response that reports a failure. -}
data ApiError
  = ApiError
  { aeReason :: !Text
  , aeCode :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)


instance FromJSON ApiError where
  parseJSON = withObject "ApiError" parseApiError


{- |
In python this was:

   if isinstance(data, dict):
       reason = data.get("errorMessage")
       reason = reason or data.get("reason")
       reason = reason or data.get("errorReason")
       if not reason and isinstance(data.get("error"), str):
           reason = data.get("error")
       if not reason and data.get("error"):
           reason = "Unknown reason"

       code = data.get("errorCode")
       if not code and data.get("serverErrorCode"):
           code = data.get("serverErrorCode")
-}
parseApiError :: Object -> Parser ApiError
parseApiError o =
  let reason = o .: "errorMessage" <|> o .: "reason" <|> o .: "errorReason" <|> orError
      hasError = member "error" o
      orError = o .: "error" <|> (if hasError then pure "unknown error" else empty)
      code = o .: "errorCode" <|> o .:? "serverErrorCode"
   in ApiError <$> reason <*> code


{- |
if the cookie jar file exists
then
  load it.
  update the cookie jar from the request and response
  save it
else
  ensure its parent directory exists
  create the cookie jar from the request and response
  save it

currently unhandled:
  cannot create directory
  cannot write due to permissions
  files exists, but data cannot be parsed
-}
updateCookieJarOf :: Session -> Response a -> Request -> IO (Response a)
updateCookieJarOf s resp req = do
  let dataPath = cookiePath (sessionTopDir s) (sessionCreds s)
  pathExists <- doesFileExist dataPath
  now <- getCurrentTime
  if pathExists
    then do
      readJar dataPath >>= \case
        Left e -> fail $ show e
        Right old -> do
          let (updated, resp_) = updateCookieJar resp req now old
          writeNetscapeJar dataPath updated
          pure resp_
    else do
      createDirectoryIfMissing True $ sessionTopDir s
      let (updated, resp_) = updateCookieJar resp req now $ createCookieJar []
      writeNetscapeJar dataPath updated
      pure resp_


{- |
if the sessionData file exists
then
  load it.
  update the session data from the headers
  save the updated data
else
  ensure its parent directory exists
  create the session data from the headers
  save it

currently unhandled:
  cannot create directory
  cannot write due to permissions
  files exists, but data cannot be parsed
-}
updateSavedHeadersOf :: Session -> [Header] -> IO ()
updateSavedHeadersOf s headers = do
  let dataPath = savedHeadersPath (sessionTopDir s) (sessionCreds s)
  pathExists <- doesFileExist dataPath
  if pathExists
    then do
      eitherDecodeFileStrict dataPath >>= \case
        Left e -> fail $ show e
        Right old -> encodeFile dataPath $ updateSavedHeaders headers old
    else do
      createDirectoryIfMissing True $ sessionTopDir s
      encodeFile dataPath $ mkSavedHeaders headers


authHeaders :: Api -> RequestHeaders
authHeaders api =
  let Api {apiSession = session, apiEndpoints = ep} = api
      Session {sessionClientId = cid, sessionSavedHdrs = sd} = session
      epHeaders = [(hOrigin, epHome ep), (hReferer, epHome ep <> "/")]
      headerOf name x = (name, toS x)
      maybeHeaderOf name = fmap (headerOf name)
      cidHeader = [(hClientId, toS cid)]
      sdHeaders =
        catMaybes
          [ maybeHeaderOf hCounter $ shCounter sd
          , maybeHeaderOf hSessionId $ shSessionId sd
          ]
   in staticHeaders <> epHeaders <> sdHeaders <> cidHeader


extendPath :: Request -> ByteString -> Request
extendPath req suffix = req {path = path req <> suffix}


toGet :: Request -> Request
toGet req = req {method = methodGet}


maybeValue :: (a -> Value) -> Maybe a -> Value
maybeValue = maybe Null


asObject :: [(Key, Value)] -> Value
asObject = Object . fromList


mkJsonRequest :: (a -> Request) -> (b -> Value) -> a -> b -> Request
mkJsonRequest mkBase mkBody baseSrc bodySrc =
  let base = mkBase baseSrc
      body = mkBody bodySrc
      encodedBody = encode body
   in base {requestBody = RequestBodyLBS encodedBody}


invoke ::
  (FromJSON a) =>
  (Endpoints -> b -> Request) ->
  Api ->
  b ->
  IO a
invoke mkReq api x =
  let Api {apiEndpoints} = api
      req = mkReq apiEndpoints x
   in jsonSessionRequest api req >>= failIfError


invokeWithAuthHdrs ::
  (FromJSON a) =>
  (Endpoints -> b -> Request) ->
  Api ->
  b ->
  IO a
invokeWithAuthHdrs mkReq api x =
  let Api {apiEndpoints} = api
      req = mkReq apiEndpoints x
      requestHeaders = authHeaders api
      req' = req {requestHeaders}
   in jsonSessionRequest api req' >>= failIfError


mkSavedHeaders :: [Header] -> SavedHeaders
mkSavedHeaders hs =
  SavedHeaders
    { shCountry = toS <$> lookup hCountry hs
    , shSessionId = toS <$> lookup hSessionId hs
    , shSessionToken = toS <$> lookup hSessionToken hs
    , shTrustToken = toS <$> lookup hTrustToken hs
    , shCounter = toS <$> lookup hCounter hs
    }


updateSavedHeaders :: [Header] -> SavedHeaders -> SavedHeaders
updateSavedHeaders hs sd =
  sd
    { shCountry = (toS <$> lookup hCountry hs) <|> shCountry sd
    , shSessionId = (toS <$> lookup hSessionId hs) <|> shSessionId sd
    , shSessionToken = (toS <$> lookup hSessionToken hs) <|> shSessionToken sd
    , shTrustToken = (toS <$> lookup hTrustToken hs) <|> shTrustToken sd
    , shCounter = (toS <$> lookup hCounter hs) <|> shCounter sd
    }


-- | The known "realms" with different 'Endpoints'.
data Realm = China | Usual
  deriving (Eq, Show)


realmEndpoints :: Realm -> Endpoints
realmEndpoints China = chinaEndpoints
realmEndpoints Usual = usualEndpoints


-- | A base URL roots and default Request used to construct other service Requests
data Endpoints = Endpoints
  { epHome :: !ByteString
  , epAuth :: !Request
  , epSetup :: !Request
  }


usualEndpoints :: Endpoints
usualEndpoints =
  Endpoints
    { epHome = "https://www.icloud.com"
    , epAuth = authReq
    , epSetup = setupReq
    }


chinaEndpoints :: Endpoints
chinaEndpoints =
  Endpoints
    { epHome = "https://www.icloud.com.cn"
    , epAuth = authReq
    , epSetup = setupReq {host = "setup.icloud.com.cn"}
    }


apiRequest :: Request
apiRequest = defaultRequest {secure = True, method = methodPost}


authReq :: Request
authReq = apiRequest {host = "idmsa.apple.com", path = "/appleauth/auth"}


setupReq :: Request
setupReq = apiRequest {host = "setup.icloud.com", path = "/setup/ws/1"}


-- | Header names used in auth and server HTTP requests
hCountry
  , hSessionId
  , hSessionToken
  , hTrustToken
  , hCounter
  , hOrigin
  , hClientId ::
    HeaderName
hCountry = mk "X-Apple-ID-Account-Country"
hSessionId = mk "X-Apple-ID-Session-Id"
hSessionToken = mk "X-Apple-Session-Token"
hTrustToken = mk "X-Apple-TwoSV-Trust-Token"
hCounter = mk "scnt"
hOrigin = mk "Origin"
hClientId = mk "X-Apple-OAuth-State"


staticHeaders :: [Header]
staticHeaders =
  [ ("X-Apple-OAuth-Client-Id", xAppleKey)
  , ("X-Apple-OAuth-Client-Type", "firstPartyAuth")
  , ("X-Apple-OAuth-Redirect-URI", "https://www.icloud.com")
  , ("X-Apple-OAuth-Require-Grant-Code", "true")
  , ("X-Apple-OAuth-Response-Mode", "web_message")
  , ("X-Apple-OAuth-Response-Type", "code")
  , ("X-Apple-Widget-Key", xAppleKey)
  ]


xAppleKey :: ByteString
xAppleKey = "d39ba9916b7251055b22c7f910e2ea796ee65e98b2ddecea8f5dde8d9d1a815d"


-- | Models the known values of password protocol
data PasswordProtocol = Old | New
  deriving (Eq, Show)


instance FromJSON PasswordProtocol where
  parseJSON =
    let fromText "s2k" = Right New
        fromText "s2k_fo" = Right Old
        fromText alt = Left $ "unknown PasswordProtocol: " ++ show alt
     in withText "PasswordProtocol" $ either fail pure . fromText


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
      iterations = o .: "iterations"
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
signinInit = invokeWithAuthHdrs signinInitReq


-- | Data used during key derivation and verification
data KeyDeriver = KeyDeriver
  { kdTag :: !Text
  , kdIterations :: !Word64
  , kdProtocol :: !PasswordProtocol
  , kdWrappedF :: !FancyPseudoRandomF
  }


instance XCalculator KeyDeriver where
  calcX = calcXUsingKeyDeriver


{--| Implements calcuation of X using PBKDF2 to derive a key from the password alone.

Also handles support for both the latest and the legacy approach to serializing
the hashed password as described
[here](https://github.com/XcodesOrg/XcodesApp/pull/650)
-}
calcXUsingKeyDeriver :: KeyDeriver -> FromClient -> FromServer -> ByteString
calcXUsingKeyDeriver kd fc fs =
  let FromServer {fsSalt, fsKnownAlgorithm = hashAlgo} = fs
      h = hashMany hashAlgo
      KeyDeriver {kdIterations = count, kdWrappedF, kdProtocol} = kd

      -- the old protocol requires base16 encoding the digest before key derivation
      useProtocol Old = Base16.encode
      useProtocol New = id
      hashed = useProtocol kdProtocol $ hashText hashAlgo $ fcPassword fc
      reallyHashed = calcPBKDF2 kdWrappedF hashed fsSalt count
   in h [fsSalt, h [":", reallyHashed]]


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


signinInitBase :: Endpoints -> Request
signinInitBase =
  let
    withQuery x = x {queryString = "?isRememberMeEnabled=true"}
   in
    withQuery . (`extendPath` "/signin/init") . epAuth


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


runSigninComplete :: (FromJSON a) => Api -> KeyDeriver -> Results -> IO a
runSigninComplete api@Api {apiSession = session} kd siResults =
  let siSavedHeaders = sessionSavedHdrs session
      siAccountName = credAccountName $ sessionCreds session
      completion =
        SigninCompletion
          { siTag = kdTag kd
          , siAccountName
          , siResults
          , siSavedHeaders
          }
   in signinComplete api completion


signinComplete :: (FromJSON a) => Api -> SigninCompletion -> IO a
signinComplete = invoke signinCompleteReq


signinCompleteReq :: Endpoints -> SigninCompletion -> Request
signinCompleteReq = mkJsonRequest signinCompleteBase signinCompleteValue


signinCompleteBase :: Endpoints -> Request
signinCompleteBase = (`extendPath` "/signin/complete") . epAuth


signinCompleteValue :: SigninCompletion -> Value
signinCompleteValue si =
  let SigninCompletion {siResults = results} = si
      toBase64 = extractBase64 . encodeBase64
      clientProof = toBase64 $ rClientProof results
      serverProof = toBase64 $ rServerProof results
      singleElem x = Array [String x]
      maybeArray = maybe (Array []) singleElem
   in asObject
        [ ("m1", String clientProof)
        , ("m2", String serverProof)
        , ("trustTokens", maybeArray (shTrustToken (siSavedHeaders si)))
        , ("rememberMe", Bool True)
        , ("accountName", String (siAccountName si))
        , ("c", String (siTag si))
        ]


twoSvTrust :: Endpoints -> Request
twoSvTrust = (`extendPath` "/2sv/trust") . toGet . epAuth


accountLogin :: Api -> IO Value
accountLogin api = invoke accountLoginReq api (sessionSavedHdrs $ apiSession api)


accountLoginReq :: Endpoints -> SavedHeaders -> Request
accountLoginReq = mkJsonRequest accountLoginBase accountLoginValue


accountLoginBase :: Endpoints -> Request
accountLoginBase = (`extendPath` "/signin/accountLoginBase") . epSetup


accountLoginValue :: SavedHeaders -> Value
accountLoginValue hs =
  asObject
    [ ("accountCountryCode", maybeValue String (shCountry hs))
    , ("dsWebAuthToken", maybeValue String (shSessionToken hs))
    , ("trustToken", maybeValue String (shTrustToken hs))
    , ("extended_login", Bool True)
    ]


validate :: Endpoints -> Request
validate = (`extendPath` "/validate") . epSetup


validateValue :: Value
validateValue = Null


validate2FA :: Endpoints -> Request
validate2FA = (`extendPath` "/verify/trusteddevice/securitycode") . epSetup


validate2FAValue :: Text -> Value
validate2FAValue code = Object [("securityCode", Object [("code", String code)])]


validateVerification :: Endpoints -> Request
validateVerification = (`extendPath` "/validateVerificationCode") . epSetup


sendVerification :: Endpoints -> Request
sendVerification = (`extendPath` "/sendVerificationCode") . epSetup


listDevices :: Endpoints -> Request
listDevices = (`extendPath` "/listDevices") . toGet . epSetup
