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
    ApiError (..)
  , ApiResponse (..)
  , ServiceError (..)
  , ServiceErrors (..)
  , SEReply ()

    -- * functions
  , extractOrFail
  )
where

import Control.Applicative (Alternative (..), (<|>))
import Data.Aeson
  ( FromJSON (..)
  , Object
  , genericParseJSON
  , withObject
  , (.:)
  , (.:?)
  )
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.KeyMap (member)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)


-- | Represents an API response that may succeed or fail with @ApiError@
data ApiResponse a = Failed !ApiError | Succeeded !a
  deriving (Eq, Show)


instance (FromJSON a) => FromJSON (ApiResponse a) where
  parseJSON v = (Failed <$> parseJSON v) <|> (Succeeded <$> parseJSON v)


{- | Obtains the success value from an ApiResponse on throws an exception in IO
reporting =  the failure
-}
extractOrFail :: ApiResponse a -> IO a
extractOrFail (Failed x) = fail $ Text.unpack $ aeReason x
extractOrFail (Succeeded x) = pure x


-- | Represents an API response that reports a failure.
data ApiError
  = ApiError
  { aeReason :: !Text
  , aeCode :: !(Maybe Text)
  }
  deriving (Eq, Show)


instance FromJSON ApiError where
  parseJSON = withObject "ApiError" parseApiError


{-
In python, this looks like:

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


showServiceErrors :: ServiceErrors -> Text
showServiceErrors (ServiceErrors Nothing) = "unexpected non-service error response"
showServiceErrors (ServiceErrors (Just xs)) = Text.concat $ map ((<> ":") . seMessage) xs


-- | A response that might contain @ServiceErrors@
newtype SEReply a = SEReply
  { unSEReply :: Either ServiceErrors a
  }
  deriving (Eq, Show, Generic)


instance (FromJSON a) => FromJSON (SEReply a) where
  parseJSON =
    let parseJSON' v = (Left <$> parseJSON v) <|> (Right <$> parseJSON v)
     in fmap SEReply . parseJSON'


-- | A container for optional @ServiceError@s
newtype ServiceErrors = ServiceErrors {unServiceErrors :: Maybe [ServiceError]}
  deriving (Eq, Show)


instance FromJSON ServiceErrors where
  parseJSON = withObject "ServiceErrors" (fmap ServiceErrors . parseServiceErrors)


parseServiceErrors :: Object -> Parser (Maybe [ServiceError])
parseServiceErrors o = o .: "service_errors"
