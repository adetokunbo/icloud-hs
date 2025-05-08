{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    ApiResponse (..)
  , ApiError (..)
  , SEReply
  , extractOrRetry

    -- * classes
  , ExtractOr (..)
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


-- | Represents an API response that reports a failure.
data ApiError
  = ApiError
  { aeReason :: !Text
  , aeCode :: !(Maybe Text)
  }
  deriving (Eq, Show)


instance FromJSON ApiError where
  parseJSON = withObject "ApiError" parseApiError


instance ExtractOr a ApiResponse where
  extractOr (Succeeded x) = pure x
  extractOr (Failed x) = fail $ Text.unpack $ aeReason x


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


-- | Represents a service error, several of which may be included in one api response
data ServiceError
  = ServiceError
  { seCode :: !Int
  , seTitle :: !Text
  , seMessage :: !Text
  }
  deriving (Eq, Show, Generic)


instance FromJSON ServiceError where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase


class IsBadCode a where
  isBadCode :: a -> Bool


instance IsBadCode ServiceError where
  isBadCode = (== "Incorrect verification code") . seMessage


instance IsBadCode [ServiceError] where
  isBadCode = any isBadCode


showServiceErrors :: ServiceErrors -> Text
showServiceErrors (ServiceErrors Nothing) = "unexpected non-service error response"
showServiceErrors (ServiceErrors (Just xs)) = Text.concat $ map ((<> ":") . seMessage) xs


-- | A response that might contain @ServiceErrors@
newtype SEReply a = SEReply
  { unSEReply :: Either ServiceErrors a
  }
  deriving (Eq, Show, Generic)


{- | Specifies a function that extracts a result from an 'SEReply' indicating if a
   retry should be attempted

a result of @Nothing@ indicates a retry is necessary
-}
extractOrRetry :: SEReply a -> IO (Maybe a)
extractOrRetry (SEReply (Right x)) = pure (Just x)
extractOrRetry (SEReply (Left se)) | isBadCode se = pure Nothing
extractOrRetry (SEReply (Left se)) = fail $ Text.unpack $ showServiceErrors se


instance (FromJSON a) => FromJSON (SEReply a) where
  parseJSON =
    let parseJSON' v = (Left <$> parseJSON v) <|> (Right <$> parseJSON v)
     in fmap SEReply . parseJSON'


-- | A container for optional @ServiceError@s
newtype ServiceErrors = ServiceErrors {unServiceErrors :: Maybe [ServiceError]}
  deriving (Eq, Show)


instance FromJSON ServiceErrors where
  parseJSON = withObject "ServiceErrors" (fmap ServiceErrors . parseServiceErrors)


instance IsBadCode ServiceErrors where
  isBadCode (ServiceErrors Nothing) = False
  isBadCode (ServiceErrors (Just xs)) = isBadCode xs


parseServiceErrors :: Object -> Parser (Maybe [ServiceError])
parseServiceErrors o = o .:? "service_errors"


-- | Specifies a function that extracts a result from a container in IO
class ExtractOr a b where
  -- | extract a result type from containing type in IO, reporting errors in IO
  extractOr :: b a -> IO a


instance ExtractOr a SEReply where
  extractOr (SEReply (Right a)) = pure a
  extractOr (SEReply (Left se)) = fail $ Text.unpack $ showServiceErrors se
