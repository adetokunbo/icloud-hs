{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : etwork.ICloud.Auth
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides functions and/or data types that support Top Sample goals
-}
module Network.ICloud.Auth (
  -- * datatypes
  Credentials (..),
  Session (..),
  clientIdPath,
  savedHeadersPath,
  cookiePath,
  SavedHeaders (..),
  Endpoints (..),

  -- * functions
  newClientId,
  sessionInit,
) where

import Data.Aeson (
  FromJSON (..),
  Options (..),
  ToJSON (..),
  eitherDecodeFileStrict,
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
  withObject,
  (.:),
 )
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath ((</>))


-- | don't derive Show to avoid the risk of logging a password
data Session = Session
  { sessionCreds :: !Credentials
  , sessionTopDir :: !FilePath
  , sessionClientId :: !Text
  , sessionSavedHdrs :: !SavedHeaders
  }
  deriving (Eq)


newClientId :: IO Text
newClientId = ("auth-" <>) . toText <$> nextRandom


savedHeadersPath :: FilePath -> Credentials -> FilePath
savedHeadersPath topDir creds = topDir </> Text.unpack (sessionBase creds)


cookiePath :: FilePath -> Credentials -> FilePath
cookiePath topDir creds = topDir </> Text.unpack (cookieBase creds)


clientIdPath :: FilePath -> Credentials -> FilePath
clientIdPath topDir creds = topDir </> Text.unpack (clientIdBase creds)


-- | don't derive Show to avoid the risk of logging a password
data Credentials = Credentials
  { credAccountName :: !Text
  -- ^ the account name is the user's AppleId, usually an email address
  , credPassword :: !Text
  -- ^ the password used to logon to ICloud
  }
  deriving (Eq)


instance FromJSON Credentials where
  parseJSON = withObject "Credentials" $ \o ->
    let accountName = o .: "accountName"
        password = o .: "password"
     in Credentials <$> accountName <*> password


sprucedName :: Credentials -> Text
sprucedName =
  let p aChar = isAlphaNum aChar || aChar == '@'
      replaceAt = Text.replace "@" "-"
   in replaceAt . Text.filter p . credAccountName


cookieBase :: Credentials -> Text
cookieBase = (<> ".cookies.txt") . sprucedName


sessionBase :: Credentials -> Text
sessionBase = (<> ".session.json") . sprucedName


clientIdBase :: Credentials -> Text
clientIdBase = (<> ".client-id.txt") . sprucedName


realmEndpoints :: Realm -> Endpoints
realmEndpoints China = chinaEndpoints
realmEndpoints Usual = defaultEndpoints


-- | The known "realms" with different Endpoints.
data Realm = China | Usual
  deriving (Eq, Show)


defaultEndpoints :: Endpoints
defaultEndpoints =
  Endpoints
    { epAuth = "https://idmsa.apple.com/appleauth/auth"
    , epHome = "https://www.icloud.com"
    , epSetup = "https://setup.icloud.com/setup/ws/1"
    }


chinaEndpoints :: Endpoints
chinaEndpoints =
  Endpoints
    { epAuth = "https://idmsa.apple.com/appleauth/auth"
    , epHome = "https://www.icloud.com.cn"
    , epSetup = "https://setup.icloud.com.cn/setup/ws/1"
    }


-- | A fixed set of HTTP URL roots used by all the service URLs
data Endpoints = Endpoints
  { epHome :: !ByteString
  , epAuth :: !ByteString
  , epSetup :: !ByteString
  }
  deriving (Eq, Show)


-- instance FromJSON Endpoints where
--   parseJSON = withObject "Endpoints" $ \o ->
--     let home = o .: "home"
--         auth = o .: "auth"
--         setup = o .: "setup"
--      in Endpoints <$> home <*> auth <*> setup

-- | Data obtained from HTTP response headers that define a user session
data SavedHeaders = SavedHeaders
  { shCountry :: !(Maybe Text)
  , shSessionId :: !(Maybe Text)
  , shSessionToken :: !(Maybe Text)
  , shTrustToken :: !(Maybe Text)
  , shCounter :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)


instance FromJSON SavedHeaders where
  parseJSON = genericParseJSON simpleOptions


instance ToJSON SavedHeaders where
  toJSON = genericToJSON simpleOptions
  toEncoding = genericToEncoding simpleOptions


emptySavedHeaders :: SavedHeaders
emptySavedHeaders = SavedHeaders Nothing Nothing Nothing Nothing Nothing


{- |  init
ignore impl
  [x]   [ ] when password not given: get from keyring
  [x]   [ ] make user dict from username and password
  [ ]   [x] when clientId not saved on filesystem: generate using UUID and save
  [x]   [ ] store bool args 'with_family' and 'verify'
  [ ]   [x] store auth, home, and setup endpoints
  [x]   [ ] setup the password filter
  [ ]   [x] ensure the cookie directory exists
  [x]   [ ] update 'session' Origin and Referer header
  [ ]   [ ]
  [ ]   [ ]

  [ ]   [ ]
-}
sessionInit :: Realm -> IO ()
sessionInit realm = do
  let _endpoints = realmEndpoints realm
  sessionTopDir <- getUserConfigDir appPath
  _session <- loadUserSession sessionTopDir >>= either fail pure
  pure ()


loadUserSession :: FilePath -> IO (Either String Session)
loadUserSession sessionTopDir = do
  let credsPath = sessionTopDir </> "credentials.json"
      withCreds sessionCreds = do
        sessionClientId <- loadClientId sessionTopDir sessionCreds
        orSavedHeaders <- loadSavedHeaders sessionTopDir sessionCreds
        case orSavedHeaders of
          Left err -> pure (Left err)
          Right sessionSavedHdrs ->
            pure $ Right Session {sessionClientId, sessionCreds, sessionTopDir, sessionSavedHdrs}
  eitherDecodeFileStrict credsPath >>= either (pure . Left) withCreds


loadSavedHeaders :: FilePath -> Credentials -> IO (Either String SavedHeaders)
loadSavedHeaders topDir creds = do
  let dataPath = savedHeadersPath topDir creds
  pathExists <- doesFileExist dataPath
  if not pathExists
    then pure $ Right emptySavedHeaders
    else eitherDecodeFileStrict dataPath


loadClientId :: FilePath -> Credentials -> IO Text
loadClientId topDir creds = do
  let dataPath = clientIdPath topDir creds
  pathExists <- doesFileExist dataPath
  if pathExists
    then Text.readFile dataPath
    else do
      anId <- newClientId
      Text.writeFile dataPath anId
      pure anId


simpleOptions :: Options
simpleOptions = aesonPrefix snakeCase


appPath :: FilePath
appPath = "hs-config-auth"
