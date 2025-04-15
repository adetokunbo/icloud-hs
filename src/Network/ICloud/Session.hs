{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Network.ICloud.Session
Copyright   : (c) 2025 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Contains data types and combinators for persisting authentication data.
-}
module Network.ICloud.Session
  ( -- * Credentials
    Credentials (..)

    -- ** paths related to @Credentials@
  , cookiePath
  , clientIdPath
  , credentialsPath
  , savedHeadersPath

    -- * Session
  , Session (..)
  , SavedHeaders (..)
  , loadSession
  , loadSavedHeaders
  , runSrpAuth
  , newClientId
  , updateSessionSavedHeaders
  , pristine

    -- * path components
  , appBase
  , (</>)
  )
where

import Control.Monad ((>=>))
import Crypto.SRP
  ( FromClient (..)
  , FromServer (..)
  , Results
  , XCalculator
  , calcResults
  )
import Data.Aeson
  ( FromJSON (..)
  , KeyValue (..)
  , Options (..)
  , ToJSON (..)
  , eitherDecodeFileStrict
  , encodeFile
  , genericParseJSON
  , genericToEncoding
  , genericToJSON
  , object
  , withObject
  , (.:)
  )
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath ((</>))


-- | Persistent data that identifies a user and their authentication state.
data Session = Session
  { sessionCreds :: !Credentials
  , sessionTopDir :: !FilePath
  , sessionClientId :: !Text
  }
  deriving
    ( Eq
      -- ^ don't derive Show to avoid the risk of logging a password
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


{- | Determine the path of file containing the credentials in the configuration
   directory
-}
credentialsPath :: FilePath -> FilePath
credentialsPath topDir = topDir </> "credentials.json"


-- | The name and password of a user
data Credentials = Credentials
  { credAccountName :: !Text
  -- ^ the account name is the user's AppleId, usually an email address
  , credPassword :: !Text
  -- ^ the password used to logon to ICloud
  }
  deriving
    ( Eq
      -- ^ don't derive Show to avoid the risk of logging a password
    )


instance FromJSON Credentials where
  parseJSON = withObject "Credentials" $ \o ->
    let accountName = o .: "accountName"
        password = o .: "password"
     in Credentials <$> accountName <*> password


instance ToJSON Credentials where
  toJSON c =
    object
      [ "password" .= credPassword c
      , "accountName" .= credAccountName c
      ]


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


-- | A @SavedHeaders@ with nothing set
pristine :: SavedHeaders
pristine = SavedHeaders Nothing Nothing Nothing Nothing Nothing


{- | Update the stored saved headers

if the sessionData file exists
then
  load it.
  update the session data from the headers
  save the updated data
else
  ensure its parent directory exists
  create the session data from the headers
  save it

not handled (thrown as IOException):
  cannot create directory
  cannot write due to permissions
  file exists, but data cannot be parsed
-}
updateSessionSavedHeaders
  :: Session
  -> (SavedHeaders -> SavedHeaders)
  -- ^ a function that modifies the session's saved headers
  -> IO ()
updateSessionSavedHeaders s modSavedHeaders = do
  let dataPath = savedHeadersPath (sessionTopDir s) (sessionCreds s)
      updateAndSave x = do
        let new = modSavedHeaders x
        encodeFile dataPath new
  pathExists <- doesFileExist dataPath
  if pathExists
    then do
      eitherDecodeFileStrict dataPath >>= either (fail . show) updateAndSave
    else do
      createDirectoryIfMissing True $ sessionTopDir s
      updateAndSave pristine


-- | Loads a @Session@ from state on the filesystem
loadSession :: IO Session
loadSession = do
  sessionTopDir <- getUserConfigDir appBase
  loadSessionOr sessionTopDir >>= either fail pure


-- | Implements the SRP authentiction sequence
runSrpAuth
  :: (XCalculator b)
  => IO FromClient
  -> (FromClient -> IO (FromServer, b))
  -> (b -> Maybe Results -> IO a)
  -> IO a
runSrpAuth mkClientSide stepOne stepTwo = do
  clientSide <- mkClientSide
  (serverSide, extra) <- stepOne clientSide
  stepTwo extra (calcResults extra clientSide serverSide)


loadCredentials :: FilePath -> IO (Either String Credentials)
loadCredentials = eitherDecodeFileStrict . credentialsPath


loadCredentials' :: FilePath -> IO (Either String (FilePath, Credentials))
loadCredentials' topDir = fmap (topDir,) <$> loadCredentials topDir


loadSession' :: Either String (FilePath, Credentials) -> IO (Either String Session)
loadSession' (Left err) = pure $ Left err
loadSession' (Right (sessionTopDir, sessionCreds)) = do
  sessionClientId <- loadClientId sessionTopDir sessionCreds
  pure $ Right Session{sessionClientId, sessionCreds, sessionTopDir}


loadSessionOr :: FilePath -> IO (Either String Session)
loadSessionOr = loadCredentials' >=> loadSession'


-- | Load the @SavedHeaders@ for this session
loadSavedHeaders :: Session -> IO SavedHeaders
loadSavedHeaders Session{sessionTopDir, sessionCreds} =
  loadSavedHeaders' sessionTopDir sessionCreds >>= either fail pure


loadSavedHeaders' :: FilePath -> Credentials -> IO (Either String SavedHeaders)
loadSavedHeaders' topDir creds = do
  let dataPath = savedHeadersPath topDir creds
  pathExists <- doesFileExist dataPath
  if not pathExists
    then pure $ Right pristine
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


appBase :: FilePath
appBase = "hs-icloud-auth"
