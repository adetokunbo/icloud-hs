{-# LANGUAGE FlexibleContexts #-}
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
  ( -- * functions
    mkApi
  , login

    -- * classes
  , AsVerifyRequest (..)
  )
where

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
  , ToJSON (..)
  , eitherDecode
  , encode
  , withObject
  , withText
  , (.:)
  )
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Types (Parser, Value (..))
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
import Network.ICloud.Http.CookieJar
import Network.ICloud.Http.Endpoints
  ( Endpoints (..)
  , Realm
  , accountLoginBase
  , extendPath
  , homeHeaders
  , realmEndpoints
  , signinCompleteBase
  , signinInitBase
  , toPut
  , twoSvTrust
  , validateBase
  , verifySecurityCodeReq
  , withAcceptJson
  , withAppleOauthHeaders
  , withBody
  , withHeaders
  , withICloudWidgetKey
  )
import Network.ICloud.Http.Errors
  ( ApiResponse
  , ExtractOr (..)
  , extractOrRetry
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
  , saveLoginMsg
  , updateSavedHeaders
  , updateSessionSavedHeaders
  )
import Network.ICloud.Trust
  ( TrustData
  , TrustedDevice (..)
  , TrustedPhone (..)
  , pleaseReadCode
  , withSelectedPhoneOrDevice
  )


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
  apiSession <- loadSession
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


-- | Logs into ICloud
login :: Api -> IO ()
login api = do
  active <- hasActiveSession api
  unless active $ do
    runApiSrpAuth api
    accountLogin api >>= saveLoginMsg (apiSession api)


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
runApiSrpAuth api@Api{apiSession, apiGroup} = do
  let Credentials
        { credAccountName = user
        , credPassword = password
        } = sessionCreds apiSession
      mkSrpClient = mkFromClient user password apiGroup
      stepOne = runSigninInit api
      stepTwo = runSigninComplete api
  runSrpAuth mkSrpClient stepOne stepTwo


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
  :: (FromJSON a) => Api -> Request -> IO (Maybe a)
callSEReply api req =
  let
    extractOrRetry' r | statusCode (responseStatus r) >= 400 = fail $ showStatusOf r
    extractOrRetry' r = extractOrRetry $ responseBody r
   in
    rawRequest api req >>= mapM asJson >>= extractOrRetry'


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
signinInit api other = do
  savedHdrs <- loadSavedHeaders (apiSession api)
  callHandlingResponse signinInitReq (withHeaders (authHeaders api savedHdrs)) extractOr' api other


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
  increaseTrust api


signinComplete :: Api -> SigninCompletion -> IO ()
signinComplete api = callHandlingResponse signinCompleteReq id (handleSigninComplete api) api


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


handleSigninComplete :: Api -> Response (ApiResponse ()) -> IO ()
handleSigninComplete api resp = do
  let code = statusCode $ responseStatus resp
      body = responseBody resp
  if
    | code == 401 -> fail "invalid username or password"
    | code == 403 -> fail "account is locked"
    | code == 412 -> fail "need to login to Apple and acknowledge the privacy agreement"
    | code == 409 -> checkAuthCode api
    | code >= 400 -> fail $ showStatusOf resp
    | otherwise -> extractOr body


-- | Performs a code verification authentication flow
checkAuthCode :: (FromJSON a) => Api -> IO a
checkAuthCode api = do
  let
    handleTwoStep = checkAuthCode' (askForTwoStepCode api) pleaseReadCode api
    handleTwoFactor = checkAuthCode' (askForTwoFactorCode api) pleaseReadCode api
  chooseTrustType api >>= withSelectedPhoneOrDevice handleTwoFactor handleTwoStep


checkAuthCode'
  :: (AsVerifyRequest b, FromJSON a)
  => (b -> IO ())
  -> IO AuthCode
  -> Api
  -> b
  -> IO a
checkAuthCode' seekAuthCode enterAuthCode api verifier = do
  let maybeRetry = maybe (checkAuthCode' seekAuthCode enterAuthCode api verifier) pure
  seekAuthCode verifier
  enterAuthCode >>= verifyCodeOrRetry api verifier >>= maybeRetry


verifyCodeOrRetry :: (FromJSON a, AsVerifyRequest b) => Api -> b -> AuthCode -> IO (Maybe a)
verifyCodeOrRetry api x code =
  let req' = verifySecurityCodeReq (verifyCodeType x) $ apiEndpoints api
      req = withBody (encode $ asVerifyRequest x code) req'
   in callSEReply api req


validate :: Api -> IO Value
validate api@Api{apiEndpoints} = callApi api (validateReq apiEndpoints) >>= extractOr'


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


increaseTrust :: Api -> IO ()
increaseTrust api = callRequiredHeaders api $ twoSvTrust (apiEndpoints api)


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


{- | The code sent to a user device that the user must enter to confirm
authenticity
-}
type AuthCode = Text


class AsVerifyRequest a where
  asVerifyRequest :: a -> AuthCode -> Value
  verifyCodeType  :: a -> Text


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
