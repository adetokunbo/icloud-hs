{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ICloud.Trust
  ( -- * data types
    CodeStatus (..)
  , TrustedPhone (..)
  , TrustedDevice (..)
  , TrustedList (..)
  , TrustData (..)
  )
where

import Data.Aeson
  ( FromJSON (..)
  , KeyValue (..)
  , Options (..)
  , SumEncoding (ObjectWithSingleField)
  , ToJSON (..)
  , Value (..)
  , genericParseJSON
  , genericToEncoding
  , genericToJSON
  , object
  , withObject
  , (.:)
  )
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.KeyMap (filterWithKey, toList)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Generics (Generic)


-- | Information describing the status of the security code verifiction
data CodeStatus = CodeStatus
  { scLength :: !Word8
  , scTooManyCodesSent :: !Bool
  , scTooManyCodesValidated :: !Bool
  , scSecurityCodeLocked :: !Bool
  , scSecurityCoolDown :: !Bool
  }
  deriving (Eq, Show, Generic)


instance FromJSON CodeStatus where
  parseJSON = genericParseJSON simpleOptions


instance ToJSON CodeStatus where
  toJSON = genericToJSON simpleOptions
  toEncoding = genericToEncoding simpleOptions


-- | Information about trusted phone number
data TrustedPhone = TrustedPhone
  { tpnId :: !Word8
  , tpnNumberWithDialCode :: !Text
  }
  deriving (Eq, Show, Generic)


instance FromJSON TrustedPhone where
  parseJSON = genericParseJSON simpleOptions


instance ToJSON TrustedPhone where
  toJSON = genericToJSON simpleOptions
  toEncoding = genericToEncoding simpleOptions


-- | Information about a trusted device
data TrustedDevice = TrustedDevice
  { tdId :: !Text
  , tdName :: !Text
  , tdModelName :: !Text
  }
  deriving (Eq, Show, Generic)


instance FromJSON TrustedDevice where
  parseJSON = genericParseJSON simpleOptions


instance ToJSON TrustedDevice where
  toJSON = genericToJSON simpleOptions
  toEncoding = genericToEncoding simpleOptions


-- | A list of @TrustedPhone@ or @TrustedDevice@
data TrustedList
  = TrustedPhoneNumbers ![TrustedPhone]
  | TrustedDevices ![TrustedDevice]
  deriving (Eq, Show, Generic)


instance FromJSON TrustedList where
  parseJSON = genericParseJSON trustedListOptions


instance ToJSON TrustedList where
  toJSON = genericToJSON trustedListOptions
  toEncoding = genericToEncoding trustedListOptions


trustedListOptions :: Options
trustedListOptions =
  ( simpleOptions
      { sumEncoding = ObjectWithSingleField
      , constructorTagModifier = camelCase
      }
  )


-- | Information used to specify a code check
data TrustData = TrustData
  { tdList :: !TrustedList
  , tdSecurityCode :: !CodeStatus
  }
  deriving (Eq, Show)


toJSONTrustData :: TrustData -> Value
toJSONTrustData td =
  let asPairs (Object o) = toList o
      asPairs _other = []
      fromSecurityCode = ["securityCode" .= tdSecurityCode td]
      fromTrustedList = asPairs $ toJSON $ tdList td
   in object $ fromSecurityCode <> fromTrustedList


parseJSONTrustData :: Value -> Parser TrustData
parseJSONTrustData = withObject "TrustData" $ \o ->
  let securityCode = o .: "securityCode"
      isListKey key _ignored = key == "trustedPhoneNumbers" || key == "trustedDevices"
      theList = parseJSON (Object $ filterWithKey isListKey o)
   in TrustData <$> theList <*> securityCode


instance ToJSON TrustData where
  toJSON = toJSONTrustData


instance FromJSON TrustData where
  parseJSON = parseJSONTrustData


simpleOptions :: Options
simpleOptions = aesonPrefix camelCase
