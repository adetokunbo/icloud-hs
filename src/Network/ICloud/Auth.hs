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
  sessionPath,
  cookiePath,
  SessionData (..),
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
  }
  deriving (Eq)


newClientId :: IO Text
newClientId = ("auth-" <>) . toText <$> nextRandom


sessionPath :: Session -> FilePath
sessionPath = sessionDataPath sessionBase


cookiePath :: Session -> FilePath
cookiePath = sessionDataPath cookieBase


clientIdPath :: Session -> FilePath
clientIdPath = sessionDataPath clientIdBase


sessionDataPath :: (Credentials -> Text) -> Session -> FilePath
sessionDataPath credPathF s = sessionTopDir s </> (Text.unpack . credPathF) (sessionCreds s)


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
data SessionData = SessionData
  { sdAccountCountry :: !(Maybe Text)
  , sdSessionId :: !(Maybe Text)
  , sdSessionToken :: !(Maybe Text)
  , sdTrustToken :: !(Maybe Text)
  , sdCounter :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)


instance FromJSON SessionData where
  parseJSON = genericParseJSON simpleOptions


instance ToJSON SessionData where
  toJSON = genericToJSON simpleOptions
  toEncoding = genericToEncoding simpleOptions


emptySessionData :: SessionData
emptySessionData = SessionData Nothing Nothing Nothing Nothing Nothing


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
  session <- loadUserSession sessionTopDir >>= either fail pure
  _sessionData <- loadSessionData session >>= either fail pure
  _client <- loadClientId session
  pure ()


loadUserSession :: FilePath -> IO (Either String Session)
loadUserSession sessionTopDir = do
  let credsPath = sessionTopDir </> "credentials.json"
      mkSession' sessionCreds = Session {sessionCreds, sessionTopDir}
  fmap mkSession' <$> eitherDecodeFileStrict credsPath


loadSessionData :: Session -> IO (Either String SessionData)
loadSessionData s = do
  let dataPath = sessionPath s
  pathExists <- doesFileExist dataPath
  if not pathExists
    then pure $ Right emptySessionData
    else eitherDecodeFileStrict dataPath


loadClientId :: Session -> IO Text
loadClientId s = do
  let dataPath = clientIdPath s
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
