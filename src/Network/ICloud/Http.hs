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
module Network.ICloud.Http
  ( -- * data types
    Endpoints (..)
  , Realm (..)

    -- * functions
  , updateSavedHeaders
  , mkApi
  , login

    -- * HTTP header names
  , hCounter
  , hCountry
  , hSessionId
  , hSessionToken
  , hTrustToken
  , hOrigin
  )
where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (unless)
import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.SRP
  ( FromClient (..)
  , FromServer (..)
  , KnownAlgorithm (SHA256)
  , PrimeGroup (G2048)
  , Results (..)
  , XCalculator (..)
  , digestSize
  , hashMany
  , hashText
  , mkFromClient
  )
import Data.Aeson
  ( FromJSON (..)
  , Key
  , Object
  , Options (..)
  , ToJSON (..)
  , eitherDecode
  , encode
  , genericParseJSON
  , genericToEncoding
  , genericToJSON
  , withObject
  , withText
  , (.:)
  )
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Types (Parser, Value (..))
import Data.Attoparsec.Cookie (readJar, writeNetscapeJar)
import Data.Base64.Types (extractBase64)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Base64 (decodeBase64Untyped, encodeBase64)
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , Response (..)
  , createCookieJar
  , defaultRequest
  , httpLbs
  , insertCookiesIntoRequest
  , updateCookieJar
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types
  ( Header
  , HeaderName
  , RequestHeaders
  , Status (..)
  , hAccept
  , hContentType
  , hReferer
  , hUserAgent
  , methodGet
  , methodPost
  , methodPut
  )
import Network.ICloud.Http.Errors
  ( ApiResponse
  , ExtractOr (..)
  , SEReply
  )
import Network.ICloud.PBKDF2 (FancyPseudoRandomF, deriveKey, wrapIO)
import Network.ICloud.Session
  ( Credentials (..)
  , SavedHeaders (..)
  , Session (..)
  , cookiePath
  , loadSavedHeaders
  , loadSession
  , pristine
  , runSrpAuth
  , updateSessionSavedHeaders
  )
import Network.ICloud.Trust
  ( TrustData
  , TrustedDevice (..)
  , TrustedPhone (..)
  , pleaseReadCode
  , withSelectedPhoneOrDevice
  )
import System.Directory (doesFileExist)


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
      apiEndpoints = realmEndpoints realm
  apiManager <- newTlsManager
  apiSession <- loadSession
  apiWrappedPseudoRF <- wrapIO SHA256.hmac $ digestSize apiHashAlgorithm
  pure
    Api
      { apiGroup = G2048
      , apiEndpoints
      , apiManager
      , apiHashAlgorithm
      , apiSession
      , apiWrappedPseudoRF
      }


-- | Logs into ICloud
login :: Api -> IO ()
login api = do
  active <- hasActiveSession api
  unless active $ do
    _completionReply <- runApiSrpAuth api
    _ <- accountLogin api
    pure ()


{- | Check if there is an active session

if @SavedHeaders@ are pristine skip and return false otherwise call
validate and return True if no errors occur
-}
hasActiveSession :: Api -> IO Bool
hasActiveSession api =
  let checkActive sh | sh == pristine = pure False
      checkActive _sh = validate api >> pure True
   in loadSavedHeaders (apiSession api) >>= checkActive


-- | Implements the SRP authentication sequence using the ICloud API
runApiSrpAuth :: Api -> IO ()
runApiSrpAuth api@Api{apiSession} = do
  let Credentials
        { credAccountName = user
        , credPassword = password
        } = sessionCreds apiSession
      mkClientSide = mkFromClient user password $ apiGroup api
      stepOne = runSigninInit api
      stepTwo = runSigninComplete api
  runSrpAuth mkClientSide stepOne stepTwo


-- | Make a session request and obtain the raw byte results
rawRequest :: Api -> Request -> IO (Response LBS.ByteString)
rawRequest = rawRequest' True


-- | Make a session request and obtain the raw byte results
rawRequest' :: Bool -> Api -> Request -> IO (Response LBS.ByteString)
rawRequest' mayRetry api req = do
  let Api{apiManager = mgr, apiSession = s} = api
      jarPath = cookiePath (sessionTopDir s) (sessionCreds s)
  resp <- usingJarCookies jarPath req $ flip httpLbs mgr
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
  unless isJson $ fail $ "response was not JSON: " ++ show theType
  mapM asJson raw


callSEReply
  :: (FromJSON a) => Api -> Request -> IO (Response (SEReply a))
callSEReply api req = rawRequest api req >>= mapM asJson


-- confirm the content-type of the response before attempting to parse
-- if it's wrong, throw  InvalidContentType
-- try to parse, if that fails, throw WrongDataType
asJson :: (FromJSON a) => LBS.ByteString -> IO a
asJson resp = case eitherDecode resp of
  Left _err -> fail "did not decode JSON response correctly"
  Right x -> pure x


extractOr' :: (ExtractOr a b) => Response (b a) -> IO a
extractOr' r | statusCode (responseStatus r) >= 400 = fail $ showStatusOf r
extractOr' r = extractOr $ responseBody r


showStatusOf :: Response a -> String
showStatusOf resp =
  let showResponse' x s | x >= 400 = "bad request:" ++ show s
      showResponse' x s | x >= 500 = "server error:" ++ show s
      showResponse' _x s = "ok:" ++ show s
      theStatus = responseStatus resp
      theCode = statusCode theStatus
   in showResponse' theCode theStatus


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
updateCookieJarOf' :: FilePath -> Response a -> Request -> IO (Response a)
updateCookieJarOf' dataPath resp req = do
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
      let (updated, resp_) = updateCookieJar resp req now $ createCookieJar []
      writeNetscapeJar dataPath updated
      pure resp_


addCookiesFromJar :: FilePath -> Request -> IO Request
addCookiesFromJar dataPath req = do
  pathExists <- doesFileExist dataPath
  if not pathExists
    then pure req
    else do
      now <- getCurrentTime
      readJar dataPath >>= \case
        Left e -> fail $ show e
        Right jar -> do
          let (req', jar') = insertCookiesIntoRequest req jar now
          writeNetscapeJar dataPath jar'
          pure req'


usingJarCookies :: FilePath -> Request -> (Request -> IO (Response b)) -> IO (Response b)
usingJarCookies cookieJarPath req doReq = do
  req' <- addCookiesFromJar cookieJarPath req
  resp <- doReq req'
  updateCookieJarOf' cookieJarPath resp req'


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
   in staticHeaders <> endpointHeaders ep <> sdHeaders <> cidHeader


requiredHeaders :: SavedHeaders -> RequestHeaders
requiredHeaders savedHdrs =
  let headerOf name x = (name, toS x)
      maybeHeaderOf name = fmap (headerOf name)
      sdHeaders =
        catMaybes
          [ maybeHeaderOf hCounter $ shCounter savedHdrs
          , maybeHeaderOf hSessionId $ shSessionId savedHdrs
          ]
   in acceptJson : widgetKey : sdHeaders


endpointHeaders :: Endpoints -> RequestHeaders
endpointHeaders ep = [(hOrigin, epHome ep), (hReferer, epHome ep <> "/")]


commonHeaders :: Endpoints -> RequestHeaders
commonHeaders ep = userAgent : endpointHeaders ep


extendPath :: Request -> ByteString -> Request
extendPath req suffix = req{path = path req <> suffix}


toGet :: Request -> Request
toGet req = req{method = methodGet}


toPut :: Request -> Request
toPut req = req{method = methodPut}


maybeValue :: (a -> Value) -> Maybe a -> Value
maybeValue = maybe Null


asObject :: [(Key, Value)] -> Value
asObject = Object . fromList


mkJsonRequest :: (a -> Request) -> (b -> Value) -> a -> b -> Request
mkJsonRequest mkBase mkBody baseSrc bodySrc =
  let body = mkBody bodySrc
   in mkJsonRequest' mkBase body baseSrc


mkJsonRequest' :: (a -> Request) -> Value -> a -> Request
mkJsonRequest' mkBase body baseSrc =
  let base = mkBase baseSrc
      encodedBody = encode body
   in base{requestBody = RequestBodyLBS encodedBody}


invoke
  :: (FromJSON a)
  => (Endpoints -> b -> Request)
  -> Api
  -> b
  -> IO a
invoke = invoke' id extractOr'


invoke'
  :: (FromJSON a)
  => (Request -> Request)
  -> (Response (ApiResponse a) -> IO a)
  -> (Endpoints -> b -> Request)
  -> Api
  -> b
  -> IO a
invoke' modReq handleResponse mkReq api@Api{apiEndpoints} x =
  callApi api (modReq $ mkReq apiEndpoints x) >>= handleResponse


invokeWithAuthHdrs
  :: (FromJSON a)
  => (Endpoints -> b -> Request)
  -> Api
  -> b
  -> IO a
invokeWithAuthHdrs mkReq api other = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  invoke' (withHeaders (authHeaders api savedHdrs)) extractOr' mkReq api other


-- | Update the @SavedHeaders@ using some response headers
updateSavedHeaders :: [Header] -> SavedHeaders -> SavedHeaders
updateSavedHeaders hs sd =
  sd
    { shCountry = (toS <$> lookup hCountry hs) <|> shCountry sd
    , shSessionId = (toS <$> lookup hSessionId hs) <|> shSessionId sd
    , shSessionToken = (toS <$> lookup hSessionToken hs) <|> shSessionToken sd
    , shTrustToken = (toS <$> lookup hTrustToken hs) <|> shTrustToken sd
    , shCounter = (toS <$> lookup hCounter hs) <|> shCounter sd
    }


-- | The known "realms" that have with different API endpoints.
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
    , epSetup = setupReq{host = "setup.icloud.com.cn"}
    }


apiRequest :: Request
apiRequest = defaultRequest{secure = True, method = methodPost}


authReq :: Request
authReq = apiRequest{host = "idmsa.apple.com", path = "/appleauth/auth"}


setupReq :: Request
setupReq = apiRequest{host = "setup.icloud.com", path = "/setup/ws/1"}


-- | Header names used in auth and server HTTP requests
hCountry
  , hSessionId
  , hSessionToken
  , hTrustToken
  , hCounter
  , hOrigin
  , hClientId
    :: HeaderName
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
  , widgetKey
  ]


xAppleKey :: ByteString
xAppleKey = "d39ba9916b7251055b22c7f910e2ea796ee65e98b2ddecea8f5dde8d9d1a815d"


browserAgent :: ByteString
browserAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36"


userAgent :: Header
userAgent = (hUserAgent, browserAgent)


widgetKey :: Header
widgetKey = ("X-Apple-Widget-Key", xAppleKey)


acceptJson :: Header
acceptJson = (hAccept, "application/json")


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
  let FromServer{fsSalt, fsKnownAlgorithm = hashAlgo} = fs
      h = hashMany hashAlgo
      KeyDeriver{kdIterations = count, kdWrappedF, kdProtocol} = kd

      -- the old protocol requires base16 encoding the digest before key derivation
      useProtocol Old = Base16.encode
      useProtocol New = id
      hashed = useProtocol kdProtocol $ hashText hashAlgo $ fcPassword fc
      reallyHashed = deriveKey kdWrappedF hashed fsSalt count
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
    withQuery x = x{queryString = "?isRememberMeEnabled=true"}
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


runSigninComplete :: Api -> KeyDeriver -> Maybe Results -> IO ()
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
      onFail = fail "the server public value was invalid"
  maybe onFail (signinComplete api . completion) mbResults


signinComplete :: Api -> SigninCompletion -> IO ()
signinComplete api = invoke' id (handleSigninComplete api) signinCompleteReq api


signinCompleteReq :: Endpoints -> SigninCompletion -> Request
signinCompleteReq = mkJsonRequest signinCompleteBase signinCompleteValue


signinCompleteBase :: Endpoints -> Request
signinCompleteBase = (`extendPath` "/signin/complete") . epAuth


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


handleSigninComplete :: Api -> Response (ApiResponse ()) -> IO ()
handleSigninComplete api resp = do
  let code = statusCode $ responseStatus resp
      body = responseBody resp
  if
    | code == 401 -> fail "invalid username or password"
    | code == 403 -> fail "account is locked"
    | code == 412 -> fail "need to login to Apple and acknowledge the privacy agreement"
    | code == 409 -> runTwoX api
    | code >= 400 -> fail $ showStatusOf resp
    | otherwise -> extractOr body


-- sends a request to determine the option
runTwoX :: (FromJSON a) => Api -> IO a
runTwoX api = do
  let handleTwoStep = runTwoStep api pleaseReadCode
      handleTwoFactor = runTwoFactor api pleaseReadCode
  twoXChoices api >>= withSelectedPhoneOrDevice handleTwoFactor handleTwoStep


validate :: Api -> IO ValidateReply
validate api@Api{apiEndpoints} = callApi api (validateReq apiEndpoints) >>= extractOr'


validateReq :: Endpoints -> Request
validateReq = mkJsonRequest' validateBase Null


validateBase :: Endpoints -> Request
validateBase ep = withHeaders (commonHeaders ep) $ (`extendPath` "/validate") $ epSetup ep


data ValidateReply = ValidateReply
  { vrIsExtendedLogin :: !Bool
  , vrHsaChallengeRequired :: !Bool
  }
  deriving (Eq, Show, Generic)


instance FromJSON ValidateReply where
  parseJSON = genericParseJSON simpleOptions


instance ToJSON ValidateReply where
  toJSON = genericToJSON simpleOptions
  toEncoding = genericToEncoding simpleOptions


twoSvTrust :: Endpoints -> Request
twoSvTrust = (`extendPath` "/2sv/trust") . toGet . epAuth


accountLogin :: Api -> IO ValidateReply
accountLogin api = do
  savedHdrs <- loadSavedHeaders $ apiSession api
  invoke accountLoginReq api savedHdrs


accountLoginReq :: Endpoints -> SavedHeaders -> Request
accountLoginReq = mkJsonRequest accountLoginBase accountLoginValue


accountLoginBase :: Endpoints -> Request
accountLoginBase = (`extendPath` "/accountLogin") . epSetup


twoXChoices :: Api -> IO TrustData
twoXChoices api@Api{apiEndpoints = ep} = callRequiredHeaders api (epAuth ep)


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


runTwoStep :: (FromJSON a) => Api -> IO Text -> TrustedDevice -> IO a
runTwoStep api receiveCode td = do
  askForTwoStepCode api td
  code <- receiveCode
  verifyCode api td code


verifyCode :: (FromJSON a, AsVerifyRequest b) => Api -> b -> Text -> IO a
verifyCode api x code =
  let body = RequestBodyLBS $ encode $ asVerifyRequest x code
      req' = verifySecurityCodeReq "phone" $ apiEndpoints api
      req = req'{requestBody = body}
   in callSEReply api req >>= extractOr'


verifySecurityCodeReq :: Text -> Endpoints -> Request
verifySecurityCodeReq codeType =
  (`extendPath` ("/verify/" <> toS codeType <> "/securitycode"))
    . withHeaders [(hContentType, "application/json")]
    . epAuth


askForTwoStepCode :: Api -> TrustedDevice -> IO ()
askForTwoStepCode api@Api{apiEndpoints = ep} td =
  let pathTail = toS $ "/" <> tdId td <> "/securitycode"
      mkTheReq = (`extendPath` pathTail) . askForTwoStepCodeBase
   in callRequiredHeaders api (mkTheReq ep)


askForTwoStepCodeBase :: Endpoints -> Request
askForTwoStepCodeBase = (`extendPath` "/verify/device") . toPut . epAuth


runTwoFactor :: (FromJSON a) => Api -> IO Text -> TrustedPhone -> IO a
runTwoFactor api receiveCode tpn = do
  askForTwoFactorCode api tpn
  code <- receiveCode
  verifyCode api tpn code


askForTwoFactorCode :: Api -> TrustedPhone -> IO ()
askForTwoFactorCode api tp = do
  let mode = fromMaybe "sms" $ tpnPushMode tp
      value =
        Object
          [ ("mode", String mode)
          , ("phoneNumber", Object [("id", toJSON (tpnId tp))])
          ]
      req' = askForTwoFactorCodeBase $ apiEndpoints api
      req = req'{requestBody = RequestBodyLBS $ encode value}
  callRequiredHeaders api req


askForTwoFactorCodeBase :: Endpoints -> Request
askForTwoFactorCodeBase =
  (`extendPath` "/verify/phone")
    . toPut
    . withHeaders [(hContentType, "application/json")]
    . epAuth


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


withHeaders :: RequestHeaders -> Request -> Request
withHeaders requestHeaders req = req{requestHeaders}


simpleOptions :: Options
simpleOptions = aesonPrefix snakeCase


class AsVerifyRequest a where
  asVerifyRequest :: a -> Text -> Value


instance AsVerifyRequest TrustedPhone where
  asVerifyRequest tpn code =
    Object
      [ ("securityCode", String code)
      , ("mode", String "sms")
      , ("phoneNumber", Object [("id", toJSON (tpnId tpn))])
      ]


instance AsVerifyRequest TrustedDevice where
  asVerifyRequest td code =
    Object
      [ ("securityCode", String code)
      , ("mode", String "sms")
      , ("phoneNumber", String (tdId td))
      ]
