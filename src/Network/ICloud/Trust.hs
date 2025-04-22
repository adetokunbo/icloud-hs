{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ICloud.Trust
  ( -- * data types
    CodeStatus (..)
  , TrustedPhone (..)
  , TrustedDevice (..)
  , TrustedList (..)
  , TrustData (..)

    -- * functions
  , selectPhone
  )
where

import Control.Monad (when)
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
import qualified Data.Text.IO as Text
import Data.Word (Word8)
import Fmt ((+|), (|+))
import GHC.Generics (Generic)
import SimplePrompt (promptNonEmpty)
import Text.Read (readMaybe)


selectPhone :: [TrustedPhone] -> IO TrustedPhone
selectPhone xs = do
  let putChoice (i, x) = Text.putStrLn $ "" +| i |+ ") " +| tpnNumberWithDialCode x |+ ""
  when (null xs) $ fail "sorry, expected to pick a trusted phone number, none to choose from"
  Text.putStrLn "Please select a trusted phone number to send a code to"
  mapM_ putChoice $ zip ([1 ..] :: [Int]) xs
  idx <- pleaseChooseN 1 (length xs)
  pure (xs !! (idx - 1))


pleaseChooseN :: Int -> Int -> IO Int
pleaseChooseN low high = do
  let readN = readMaybe <$> promptNonEmpty "> "
  Text.putStrLn $ "Please choose from [" +| low |+ " - " +| high |+ "]"
  readN >>= \case
    Nothing -> pleaseChooseN low high
    Just x | x < low || x > high -> pleaseChooseN low high
    Just x -> pure x


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
  , tpnPushMode :: !(Maybe Text)
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
  , tdNoTrustedDevices :: !Bool
  }
  deriving (Eq, Show)


toJSONTrustData :: TrustData -> Value
toJSONTrustData td =
  let asPairs (Object o) = toList o
      asPairs _other = []
      fromOthers =
        [ "securityCode" .= tdSecurityCode td
        , "noTrustedDevices" .= tdNoTrustedDevices td
        ]
      fromTrustedList = asPairs $ toJSON $ tdList td
   in object $ fromOthers <> fromTrustedList


parseJSONTrustData :: Value -> Parser TrustData
parseJSONTrustData = withObject "TrustData" $ \o ->
  let securityCode = o .: "securityCode"
      tdNoTrustedDevices = o .: "noTrustedDevices"
      isListKey key _ignored = key == "trustedPhoneNumbers" || key == "trustedDevices"
      theList = parseJSON (Object $ filterWithKey isListKey o)
   in TrustData <$> theList <*> securityCode <*> tdNoTrustedDevices


instance ToJSON TrustData where
  toJSON = toJSONTrustData


instance FromJSON TrustData where
  parseJSON = parseJSONTrustData


simpleOptions :: Options
simpleOptions = aesonPrefix camelCase
