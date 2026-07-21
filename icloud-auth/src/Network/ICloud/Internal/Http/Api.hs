{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

module Network.ICloud.Internal.Http.Api
  ( -- * API handle
    ApiLogger (..)
  , Api (..)
  , mkApi
  , mkApiWith
  , withLogger
  , fileLogger
  , verboseLogger
  , redactingLogger

    -- * Authenticated HTTP
  , rawRequest
  , rawRequest'
  , callApi
  , asJson
  , extractOr'
  , showStatusOf

    -- * Header helpers
  , authHeaders
  , requiredHeaders
  , callRequiredHeaders

    -- * Request builders
  , maybeValue
  , asObject
  , mkJsonRequest
  , withJsonRequestHeaders
  , callHandlingResponse

    -- * Constants
  , hClientId

    -- * Types
  , AuthCode
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless, when)
import qualified Crypto.Hash.SHA256 as SHA256
import Crypto.SRP
  ( KnownAlgorithm (SHA256)
  , PrimeGroup (G2048)
  , digestSize
  )
import Data.Aeson
  ( FromJSON (..)
  , Key
  , eitherDecode
  , encode
  )
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Types (Value (..))
import Data.ByteString (ByteString, isPrefixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk, original)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
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
  ( homeHeaders
  , withAcceptJson
  , withAppleOauthHeaders
  , withBody
  , withHeaders
  , withWidgetKey
  )
import Network.ICloud.Internal.Http
  ( hCounter
  , hSessionId
  , needsRetry
  )
import Network.ICloud.Internal.HttpErrors
  ( ApiResponse
  , AuthError (..)
  , extractOr
  )
import Network.ICloud.Internal.PBKDF2 (FancyPseudoRandomF, wrapIO)
import Network.ICloud.Internal.Session
  ( SavedHeaders (..)
  , cookiePath
  , loadSavedHeaders
  , updateSavedHeaders
  , updateSessionSavedHeaders
  )
import Network.ICloud.Session (Session (..))
import qualified Network.ICloud.Session as Session
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

__Security warning:__ all request and response headers are written verbatim,
including @Set-Cookie@, @X-Apple-Session-Token@, @X-Apple-TwoSV-Trust-Token@,
and @scnt@. Log files produced by this logger may contain live session tokens.
Use 'redactingLogger' when the log destination is not fully trusted.

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

__Security warning:__ carries the same token-exposure risk as 'fileLogger' and
additionally logs request bodies, which may contain passwords or SRP parameters.
Use 'redactingLogger' when the log destination is not fully trusted.
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


{- | Like 'fileLogger' but replaces the values of sensitive headers with
@\<redacted\>@ before writing.

The following headers are redacted in both request and response:
@Set-Cookie@, @Cookie@, @X-Apple-Session-Token@, @X-Apple-TwoSV-Trust-Token@,
@scnt@, @Authorization@.

Safe to write to shared or untrusted log destinations.
-}
redactingLogger :: Handle -> ApiLogger
redactingLogger h = ApiLogger $ \req resp -> do
  now <- getCurrentTime
  let scheme = if secure req then "https" else "http" :: String
      uri = scheme <> "://" <> toS (host req) <> toS (path req)
      status = statusCode (responseStatus resp)
      summary = show now <> " " <> toS (method req) <> " " <> uri <> " " <> show status
      fmtHdr (name, val) = toS (original name) <> ": " <> toS val
  hPutStrLn h summary
  mapM_ (hPutStrLn h . fmtHdr . redactHeader) (requestHeaders req)
  hPutStrLn h ""
  mapM_ (hPutStrLn h . fmtHdr . redactHeader) (responseHeaders resp)
  hPutStrLn h ""
  LBS.hPutStr h (responseBody resp)
  hPutStrLn h "\n---"


sensitiveHeaderNames :: Set.Set HeaderName
sensitiveHeaderNames =
  Set.fromList
    [ mk "Set-Cookie"
    , mk "Cookie"
    , mk "X-Apple-Session-Token"
    , mk "X-Apple-TwoSV-Trust-Token"
    , mk "scnt"
    , mk "Authorization"
    ]


redactHeader :: (HeaderName, BS.ByteString) -> (HeaderName, BS.ByteString)
redactHeader (name, val)
  | name `Set.member` sensitiveHeaderNames = (name, "<redacted>")
  | otherwise = (name, val)


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
   in withAppleOauthHeaders (epWidgetKey ep) $ homeHeaders ep <> sdHeaders <> cidHeader


requiredHeaders :: ByteString -> SavedHeaders -> RequestHeaders
requiredHeaders key savedHdrs =
  let headerOf name x = (name, toS x)
      maybeHeaderOf name = fmap (headerOf name)
      sdHeaders =
        catMaybes
          [ maybeHeaderOf hCounter $ shCounter savedHdrs
          , maybeHeaderOf hSessionId $ shSessionId savedHdrs
          ]
   in withAcceptJson . withWidgetKey key $ sdHeaders


callRequiredHeaders :: (FromJSON a) => Api -> Request -> IO a
callRequiredHeaders api@Api{apiSession = s, apiEndpoints = ep} req = do
  savedHdrs <- loadSavedHeaders s
  callApi api (withHeaders (requiredHeaders (epWidgetKey ep) savedHdrs) req) >>= extractOr'


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


{- | The code sent to a user device that the user must enter to confirm
authenticity
-}
type AuthCode = Text
