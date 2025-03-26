{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

  -- * class definitions

  -- * type aliases

  -- * type family extensions

  -- * functions
  rawRequest,
  jsonSessionRequest,
  mkSavedHeaders,

  -- * HTTP header names
  hCounter,
  hCountry,
  hSessionId,
  hSessionToken,
  hTrustToken,
  hOrigin,

  -- * module re-exports
) where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (unless)
import Data.Aeson (
  FromJSON (..),
  Object,
  eitherDecode,
  eitherDecodeFileStrict,
  encodeFile,
  withObject,
  (.:),
 )
import Data.Aeson.KeyMap (member)
import Data.Aeson.Types (Parser, (.:?))
import Data.Attoparsec.Cookie (readJar, writeNetscapeJar)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.Maybe (catMaybes)
import Data.String.Conv (toS)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Client (
  Manager,
  Request,
  Response (..),
  createCookieJar,
  httpLbs,
  updateCookieJar,
 )
import Network.HTTP.Types (Header, HeaderName, RequestHeaders, hContentType, hReferer)
import Network.ICloud.Auth (
  Endpoints (..),
  SavedHeaders (..),
  Session (..),
  cookiePath,
  sessionPath,
 )
import System.Directory (createDirectoryIfMissing, doesFileExist)


-- | Make a session request and obtain the raw byte results
rawRequest :: Manager -> Session -> Request -> IO (Response LBS.ByteString)
rawRequest mgr s req = do
  resp <- httpLbs req mgr
  resp' <- updateCookieJarOf s resp req
  updateSavedHeadersOf s $ responseHeaders resp'
  pure resp'


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
  (FromJSON a) => Manager -> Session -> Request -> IO (Response (ApiResponse a))
jsonSessionRequest mgr s req = do
  raw <- rawRequest mgr s req
  let isJsonType "application/json" = True
      isJsonType "text/json" = True
      isJsonType _other = False
      theType = lookup hContentType $ responseHeaders raw
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
  let dataPath = cookiePath s
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
  let dataPath = sessionPath s
  pathExists <- doesFileExist dataPath
  if pathExists
    then do
      eitherDecodeFileStrict dataPath >>= \case
        Left e -> fail $ show e
        Right old -> encodeFile dataPath $ updateSavedHeaders headers old
    else do
      createDirectoryIfMissing True $ sessionTopDir s
      encodeFile dataPath $ mkSavedHeaders headers


authHeaders :: Text -> Endpoints -> SavedHeaders -> RequestHeaders
authHeaders cid ep sd =
  let epHeaders = [(hOrigin, epHome ep), (hReferer, epHome ep <> "/")]
      headerOf name x = (name, toS x)
      maybeHeaderOf name = fmap (headerOf name)
      cidHeader = [(hClientId, toS cid)]
      sdHeaders =
        catMaybes
          [ maybeHeaderOf hCounter $ shCounter sd
          , maybeHeaderOf hSessionId $ shSessionId sd
          ]
   in staticHeaders <> epHeaders <> sdHeaders <> cidHeader


-- | Header used in auth and server HTTP requests
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
