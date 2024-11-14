{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.ICloud.Auth
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Provides functions and/or data types that support Top Sample goals
-}
module Network.ICloud.Auth (
  -- * datatypes
  Credentials (..),
  Session (..),
  sessionPath,
  cookiePath,
  SessionData (..),
  mkSessionData,

  -- * HTTP header names
  hCounter,
  hCountry,
  hSessionId,
  hSessionToken,
  hTrustToken,
) where

import Data.CaseInsensitive (mk)
import Data.Char (isAlphaNum)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Types (Header, HeaderName)
import System.FilePath ((</>))


-- | don't derive Show to avoid the risk of logging a password
data Session = Session
  { sessionCreds :: !Credentials
  , sessionTopDir :: !FilePath
  }
  deriving (Eq)


sessionPath :: Session -> FilePath
sessionPath = sessionStatePath sessionBase


cookiePath :: Session -> FilePath
cookiePath = sessionStatePath cookieBase


sessionStatePath :: (Credentials -> Text) -> Session -> FilePath
sessionStatePath credPathF s = sessionTopDir s </> (Text.unpack . credPathF) (sessionCreds s)


-- | don't derive Show to avoid the risk of logging a password
data Credentials = Credentials
  { credAccountName :: !Text
  -- ^ the account name is  the user's AppleId, usually an email address
  , credPassword :: !Text
  -- ^ the password used to logon to ICloud
  }
  deriving (Eq)


sprucedName :: Credentials -> Text
sprucedName =
  let p aChar = isAlphaNum aChar || aChar == '@'
      replaceAt = Text.replace "@" "-"
   in replaceAt . Text.filter p . credAccountName


cookieBase :: Credentials -> Text
cookieBase = (<> ".cookies.txt") . sprucedName


sessionBase :: Credentials -> Text
sessionBase = (<> ".session.json") . sprucedName


-- | Data obtained from HTTP response headers that define a user session
data SessionData = SessionData
  { sdAccountCountry :: !(Maybe Text)
  , sdSessionId :: !(Maybe Text)
  , sdSessionToken :: !(Maybe Text)
  , sdTrustToken :: !(Maybe Text)
  , sdCounter :: !(Maybe Text)
  }
  deriving (Eq, Show)


hCountry, hSessionId, hSessionToken, hTrustToken, hCounter :: HeaderName
hCountry = mk "X-Apple-ID-Account-Country"
hSessionId = mk "X-Apple-ID-Session-Id"
hSessionToken = mk "X-Apple-Session-Token"
hTrustToken = mk "X-Apple-TwoSV-Trust-Token"
hCounter = mk "scnt"


mkSessionData :: [Header] -> SessionData
mkSessionData hs =
  SessionData
    { sdAccountCountry = toS <$> lookup hCountry hs
    , sdSessionId = toS <$> lookup hSessionId hs
    , sdSessionToken = toS <$> lookup hSessionToken hs
    , sdTrustToken = toS <$> lookup hTrustToken hs
    , sdCounter = toS <$> lookup hCounter hs
    }
