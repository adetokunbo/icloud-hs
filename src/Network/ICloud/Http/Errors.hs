{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.ICloud.Http.Errors
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Datatypes that model the structured errors returned by the ICloud API.
-}
module Network.ICloud.Http.Errors
  ( -- * data types
    ServiceError (..)
  , ServiceErrors (..)
  , ServiceErrorReply (..)
  )
where

import Data.Aeson
  ( FromJSON (..)
  , Object
  , genericParseJSON
  , withObject
  , (.:)
  )
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)


-- | Represents a service error, multiple of which might occcur in one api response
data ServiceError
  = ServiceError
  { seCode :: !Int
  , seTitle :: !Text
  , seMessage :: !Text
  }
  deriving (Eq, Show, Generic)


instance FromJSON ServiceError where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase


-- | A response that might contain a  @ServiceErrors@
newtype ServiceErrorReply a = ServiceErrorReply
  { unServiceErrorReply :: Either ServiceErrors a
  }
  deriving (Eq, Show, Generic)


instance (FromJSON a) => FromJSON (ServiceErrorReply a)


-- | A container for optional @ServiceError@s
newtype ServiceErrors = ServiceErrors {unServiceErrors :: Maybe [ServiceError]}
  deriving (Eq, Show)


instance FromJSON ServiceErrors where
  parseJSON = withObject "ServiceErrors" (fmap ServiceErrors . parseServiceErrors)


parseServiceErrors :: Object -> Parser (Maybe [ServiceError])
parseServiceErrors o = o .: "service_errors"
