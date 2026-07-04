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
  , AuthError (..)
  , SEReply
  , extractOrRetry

    -- * classes
  , ExtractOr (..)
  )
where

import Control.Applicative (Alternative (..), (<|>))
import Control.Exception (Exception, throwIO)
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


-- | Structured errors thrown by the ICloud authentication layer
data AuthError
  = InvalidCredentials
  | AccountLocked
  | PrivacyAgreementRequired
  | ServiceError !Text !(Maybe Text)
  | UnexpectedResponse !Text
  deriving (Eq, Show)


instance Exception AuthError


instance ExtractOr a ApiResponse where
  extractOr (Succeeded x) = pure x
  extractOr (Failed x) = throwIO $ ServiceError (aeReason x) (aeCode x)


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


data SvcError
  = SvcError
  { seCode :: !Int
  , seTitle :: !Text
  , seMessage :: !Text
  }
  deriving (Eq, Show, Generic)


instance FromJSON SvcError where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase


class IsBadCode a where
  isBadCode :: a -> Bool


instance IsBadCode SvcError where
  isBadCode = (== "Incorrect verification code") . seMessage


instance IsBadCode [SvcError] where
  isBadCode = any isBadCode


showSvcErrors :: SvcErrors -> Text
showSvcErrors (SvcErrors Nothing) = "unexpected non-service error response"
showSvcErrors (SvcErrors (Just xs)) = Text.concat $ map ((<> ":") . seMessage) xs


-- | A response that might contain service errors
newtype SEReply a = SEReply
  { unSEReply :: Either SvcErrors a
  }
  deriving (Eq, Show, Generic)


{- | Specifies a function that extracts a result from an 'SEReply' indicating if a
   retry should be attempted

a result of @Nothing@ indicates a retry is necessary
-}
extractOrRetry :: SEReply a -> IO (Maybe a)
extractOrRetry (SEReply (Right x)) = pure (Just x)
extractOrRetry (SEReply (Left se)) | isBadCode se = pure Nothing
extractOrRetry (SEReply (Left se)) = throwIO $ ServiceError (showSvcErrors se) Nothing


instance (FromJSON a) => FromJSON (SEReply a) where
  parseJSON =
    let parseJSON' v = (Left <$> parseJSON v) <|> (Right <$> parseJSON v)
     in fmap SEReply . parseJSON'


newtype SvcErrors = SvcErrors {unSvcErrors :: Maybe [SvcError]}
  deriving (Eq, Show)


instance FromJSON SvcErrors where
  parseJSON = withObject "SvcErrors" (fmap SvcErrors . parseSvcErrors)


instance IsBadCode SvcErrors where
  isBadCode (SvcErrors Nothing) = False
  isBadCode (SvcErrors (Just xs)) = isBadCode xs


parseSvcErrors :: Object -> Parser (Maybe [SvcError])
parseSvcErrors o = o .:? "service_errors"


-- | Specifies a function that extracts a result from a container in IO
class ExtractOr a b where
  -- | extract a result type from containing type in IO, reporting errors in IO
  extractOr :: b a -> IO a


instance ExtractOr a SEReply where
  extractOr (SEReply (Right a)) = pure a
  extractOr (SEReply (Left se)) = throwIO $ ServiceError (showSvcErrors se) Nothing
