{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.ICloud.Internal.HttpErrors
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

Datatypes that model the structured errors returned by the iCloud API.
-}
module Network.ICloud.Internal.HttpErrors
  ( -- * API response wrapper
    ApiResponse (..)

    -- * API error embedded in 'ApiResponse'
  , ApiError (..)

    -- * Public exception type
  , AuthError (..)

    -- * Extracting results
  , extractOr
  )
where

import Control.Applicative (Alternative (..), (<|>))
import Control.Exception (Exception, throwIO)
import Data.Aeson
  ( FromJSON (..)
  , Object
  , withObject
  , (.:)
  , (.:?)
  )
import Data.Aeson.KeyMap (member)
import Data.Aeson.Types (Parser)
import Data.Text (Text)


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


-- | Structured errors thrown by the iCloud authentication layer
data AuthError
  = -- | The supplied credentials were rejected.
    InvalidCredentials
  | -- | The account has been locked due to too many failed sign-in attempts.
    AccountLocked
  | -- | The server requires the user to accept updated privacy terms before continuing.
    PrivacyAgreementRequired
  | -- | The API returned a structured service error with a reason and an optional error code.
    ServiceError !Text !(Maybe Text)
  | -- | An HTTP response that could not be interpreted; the 'Text' describes the failure.
    UnexpectedResponse !Text
  deriving (Eq, Show)


instance Exception AuthError


-- | Extract the result from an 'ApiResponse', throwing 'ServiceError' on failure.
extractOr :: ApiResponse a -> IO a
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
