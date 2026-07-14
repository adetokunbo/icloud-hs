{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Network.ICloud.Internal.Session
  ( -- * Credentials
    Credentials (..)

    -- ** paths related to @Credentials@
  , cookiePath
  , clientIdPath
  , credentialsPath
  , savedHeadersPath
  , loginMsgPath

    -- * Session
  , Session (..)
  , SavedHeaders (..)
  , loadSession
  , saveCredentials
  , saveCredentialsTo
  , loadSavedHeaders
  , updateSessionSavedHeaders
  , updateSavedHeaders
  , pristine
  , saveLoginMsg

    -- * AccountData
  , AccountData (..)
  , accountDataRequires2FA
  , accountDataRequires2SA
  , unknownAccountData
  , accountDataPath
  , saveAccountData
  , loadAccountData

    -- * path components
  , appBase
  , (</>)
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM, (>=>))
import Data.Aeson
  ( FromJSON (..)
  , KeyValue (..)
  , Options (..)
  , ToJSON (..)
  , Value
  , eitherDecodeFileStrict
  , encode
  , encodeFile
  , genericParseJSON
  , genericToEncoding
  , genericToJSON
  , object
  , withObject
  , (.:)
  , (.:?)
  )
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Network.HTTP.Types.Header (Header)
import Network.ICloud.Internal.Http
  ( hCounter
  , hCountry
  , hSessionId
  , hSessionToken
  , hTrustToken
  )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.FilePath ((</>))


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


{- | Persistent data identifying a user and their local authentication state.

Holds the credentials used to authenticate, the directory where session files
are stored (cookies, saved headers, account data), and the per-client OAuth
state identifier.
-}
data Session = Session
  { sessionCreds :: !Credentials
  -- ^ the credentials used to authenticate
  , sessionTopDir :: !FilePath
  -- ^ directory where session files (cookies, headers, account data) are stored
  , sessionClientId :: !Text
  -- ^ per-client OAuth state identifier sent with each request
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


-- | Determine the path of file to save the message the api returns on logon
loginMsgPath :: FilePath -> Credentials -> FilePath
loginMsgPath topDir creds = topDir </> Text.unpack (loginMsgBase creds)


-- | Save the login message to user specific filepath
saveLoginMsg :: Session -> Value -> IO ()
saveLoginMsg Session{sessionCreds = creds, sessionTopDir = topDir} = saveValue (loginMsgPath topDir creds)


{- | Structured account information returned by the account-login endpoint.

The 'adHsaVersion' field determines which two-factor flow applies:

* @0@ — unknown (used as a sentinel when no account data is available)
* @1@ — legacy two-step authentication (2SA); handled via the setup endpoint
* @2@ — modern two-factor authentication (2FA); handled via the auth endpoint
-}
data AccountData = AccountData
  { adHsaVersion :: !Int
  -- ^ HSA protocol version; drives the two-factor flow selection
  , adHsaChallengeRequired :: !Bool
  -- ^ @True@ when a 2FA challenge must be completed before access is granted
  , adHsaTrustedBrowser :: !Bool
  -- ^ @True@ when this session is already trusted and no challenge is needed
  , adWebservices :: !(Map Text Text)
  -- ^ map of webservice name to base URL, e.g. @"findme" -> "https://…"@
  }
  deriving (Eq, Show, Generic)


instance FromJSON AccountData where
  parseJSON = withObject "AccountData" $ \o -> do
    dsInfo <- o .: "dsInfo"
    adHsaVersion <- withObject "dsInfo" (.: "hsaVersion") dsInfo
    adHsaChallengeRequired <- o .:? "hsaChallengeRequired" >>= maybe (pure False) pure
    adHsaTrustedBrowser <- o .:? "hsaTrustedBrowser" >>= maybe (pure False) pure
    adWebservices <- do
      mbWs <- o .:? "webservices"
      maybe (pure Map.empty) (withObject "webservices" parseWebservices) mbWs
    pure AccountData{adHsaVersion, adHsaChallengeRequired, adHsaTrustedBrowser, adWebservices}
   where
    parseWebservices obj = do
      let pairs = KeyMap.toAscList obj
      urlPairs <- forM pairs $ \(k, v) ->
        withObject "webservice" (\sv -> fmap (AesonKey.toText k,) <$> sv .:? "url") v
      pure $ Map.fromList $ catMaybes urlPairs


instance ToJSON AccountData where
  toJSON AccountData{adHsaVersion, adHsaChallengeRequired, adHsaTrustedBrowser, adWebservices} =
    object
      [ "dsInfo" .= object ["hsaVersion" .= adHsaVersion]
      , "hsaChallengeRequired" .= adHsaChallengeRequired
      , "hsaTrustedBrowser" .= adHsaTrustedBrowser
      , "webservices" .= fmap (\url -> object ["url" .= url]) adWebservices
      ]


-- | True when full 2FA (auth-endpoint) challenge is required
accountDataRequires2FA :: AccountData -> Bool
accountDataRequires2FA ad =
  adHsaVersion ad == 2 && (adHsaChallengeRequired ad || not (adHsaTrustedBrowser ad))


-- | True when legacy 2SA (setup-endpoint) challenge is required
accountDataRequires2SA :: AccountData -> Bool
accountDataRequires2SA ad = adHsaVersion ad == 1


-- | Sentinel used when no saved @AccountData@ is available
unknownAccountData :: AccountData
unknownAccountData =
  AccountData
    { adHsaVersion = 0
    , adHsaChallengeRequired = False
    , adHsaTrustedBrowser = False
    , adWebservices = Map.empty
    }


accountDataBase :: Credentials -> Text
accountDataBase = (<> ".account-data.json") . sprucedName


-- | Determine the path of the saved account-data file for the given credentials
accountDataPath :: FilePath -> Credentials -> FilePath
accountDataPath topDir creds = topDir </> Text.unpack (accountDataBase creds)


-- | Persist @AccountData@ to the session's filesystem location
saveAccountData :: Session -> AccountData -> IO ()
saveAccountData Session{sessionCreds = creds, sessionTopDir = topDir} =
  encodeFile (accountDataPath topDir creds)


-- | Load persisted @AccountData@; returns @Nothing@ if the file is absent
loadAccountData :: Session -> IO (Maybe AccountData)
loadAccountData Session{sessionCreds = creds, sessionTopDir = topDir} = do
  let path = accountDataPath topDir creds
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else eitherDecodeFileStrict path >>= either (const (pure Nothing)) (pure . Just)


{- | Determine the path of file containing the credentials in the configuration
   directory
-}
credentialsPath :: FilePath -> FilePath
credentialsPath topDir = topDir </> "credentials.json"


{- | Write 'Credentials' to @$XDG_CONFIG_HOME\/hs-icloud-auth\/credentials.json@,
creating the directory if it does not exist.
-}
saveCredentials :: Credentials -> IO ()
saveCredentials creds = getUserConfigDir appBase >>= (`saveCredentialsTo` creds)


-- | Write 'Credentials' to @credentials.json@ inside @topDir@, creating @topDir@ if absent.
saveCredentialsTo :: FilePath -> Credentials -> IO ()
saveCredentialsTo topDir creds = do
  createDirectoryIfMissing True topDir
  encodeFile (credentialsPath topDir) creds


{- | The Apple ID and password used to sign in to iCloud.

Expected to be read from
@$XDG_CONFIG_HOME\/hs-icloud-auth\/credentials.json@ with the fields
@accountName@ and @password@.
-}
data Credentials = Credentials
  { credAccountName :: !Text
  -- ^ the Apple ID; typically an email address
  , credPassword :: !Text
  -- ^ the iCloud account password
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


loginMsgBase :: Credentials -> Text
loginMsgBase = (<> ".last-logon.json") . sprucedName


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
      updateAndSave = encodeFile dataPath . modSavedHeaders
      loadLast False = pure pristine
      loadLast True = eitherDecodeFileStrict dataPath >>= either (fail . show) pure

  doesFileExist dataPath >>= loadLast >>= updateAndSave


{- | Load a 'Session' from the local filesystem.

Reads 'Credentials' from
@$XDG_CONFIG_HOME\/hs-icloud-auth\/credentials.json@ and initialises the
session working directory (creating it if absent). A per-client ID is read
from disk if one exists, or generated and saved for future runs.

Throws an 'IOError' if the credentials file is absent or cannot be parsed.
-}
loadSession :: IO Session
loadSession = do
  sessionTopDir <- getUserConfigDir appBase
  createDirectoryIfMissing True sessionTopDir
  loadSessionOr sessionTopDir >>= either fail pure


-- | Saves a JSON @Value@ to @filepath@
saveValue :: FilePath -> Value -> IO ()
saveValue fp v = LBS.writeFile fp $ encode v


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
