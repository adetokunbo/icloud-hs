{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : etwork.ICloud.Auth
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides functions and/or data types that sync authentical credentials with the
filesystem
-}
module Network.ICloud.Auth (
  -- * Credentials
  Credentials (..),

  -- ** compute paths that depend on @Credentials@
  clientIdPath,
  savedHeadersPath,
  cookiePath,

  -- * Session
  Session (..),
  SavedHeaders (..),
  loadSession,
  runSrpAuth,

  -- * clientID generation
  newClientId,
) where

import Control.Monad ((>=>))
import Crypto.SRP (
  FromClient (..),
  FromServer (..),
  Results,
  XCalculator,
  calcResults,
 )
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


-- | Persistent data that identifies a user and their authentication state.
data Session = Session
  { sessionCreds :: !Credentials
  , sessionTopDir :: !FilePath
  , sessionClientId :: !Text
  , sessionSavedHdrs :: !SavedHeaders
  }
  deriving
    ( -- | don't derive Show to avoid the risk of logging a password
      Eq
    )


-- | Generates a new client ID.
newClientId :: IO Text
newClientId = ("auth-" <>) . toText <$> nextRandom


{- | Determine the path of file containing the HTTP response headers to be
preserved to maintain a user's authentication state
-}
savedHeadersPath :: FilePath -> Credentials -> FilePath
savedHeadersPath topDir creds = topDir </> Text.unpack (sessionBase creds)


-- | Determine the Cookie Jar file for user with the given credentials
cookiePath :: FilePath -> Credentials -> FilePath
cookiePath topDir creds = topDir </> Text.unpack (cookieBase creds)


{- | Determine the path of file containing the client ID for user with the given
credentials
-}
clientIdPath :: FilePath -> Credentials -> FilePath
clientIdPath topDir creds = topDir </> Text.unpack (clientIdBase creds)


-- | The name and password of a user
data Credentials = Credentials
  { credAccountName :: !Text
  -- ^ the account name is the user's AppleId, usually an email address
  , credPassword :: !Text
  -- ^ the password used to logon to ICloud
  }
  deriving
    ( -- | don't derive Show to avoid the risk of logging a password
      Eq
    )


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


-- | Loads a @Session@ from state on the filesystem
loadSession :: IO Session
loadSession = do
  sessionTopDir <- getUserConfigDir appPath
  loadSessionOr sessionTopDir >>= either fail pure


-- | Implements the SRP authentiction sequence
runSrpAuth ::
  (XCalculator b) =>
  IO FromClient ->
  (FromClient -> IO (FromServer, b)) ->
  (b -> Results -> IO a) ->
  IO a
runSrpAuth mkClientSide stepOne stepTwo = do
  clientSide <- mkClientSide
  (serverSide, extra) <- stepOne clientSide
  stepTwo extra (calcResults extra clientSide serverSide)


loadCredentials :: FilePath -> IO (Either String Credentials)
loadCredentials topDir = do
  let credsPath = topDir </> "credentials.json"
  eitherDecodeFileStrict credsPath


loadCredentials' :: FilePath -> IO (Either String (FilePath, Credentials))
loadCredentials' topDir = fmap (topDir,) <$> loadCredentials topDir


loadSession' :: Either String (FilePath, Credentials) -> IO (Either String Session)
loadSession' (Left err) = pure $ Left err
loadSession' (Right (sessionTopDir, sessionCreds)) = do
  sessionClientId <- loadClientId sessionTopDir sessionCreds
  orSavedHeaders <- loadSavedHeaders sessionTopDir sessionCreds
  case orSavedHeaders of
    Left err -> pure (Left err)
    Right sessionSavedHdrs ->
      pure $ Right Session {sessionClientId, sessionCreds, sessionTopDir, sessionSavedHdrs}


loadSessionOr :: FilePath -> IO (Either String Session)
loadSessionOr = loadCredentials' >=> loadSession'


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
