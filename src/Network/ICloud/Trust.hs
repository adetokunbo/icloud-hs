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
  , withSelectedPhoneOrDevice
  , pleaseReadCode
  , pleaseChooseN
  , selectPhone
  , selectDevice
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
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Word (Word8)
import Fmt ((+|), (|+))
import GHC.Generics (Generic)
import SimplePrompt (promptNonEmpty)
import Text.Read (readMaybe)


putDeviceChoice :: (Int, TrustedDevice) -> IO ()
putDeviceChoice (i, td) | tdModelName td == "" = Text.putStrLn $ "" +| i |+ ") " +| tdName td |+ "\tSMS\t" +| tdId td |+ ""
putDeviceChoice (i, td) = Text.putStrLn $ "" +| i |+ ") " +| tdName td |+ "\t" +| tdModelName td |+ "\t" +| tdId td |+ ""


selectDevice :: [TrustedDevice] -> IO TrustedDevice
selectDevice xs = do
  when (null xs) $ fail "sorry, expected to pick a trusted device, none to choose from"
  Text.putStrLn "Please select a trusted device to send a code to"
  mapM_ putDeviceChoice $ zip ([1 ..] :: [Int]) xs
  idx <- pleaseChooseN 1 (length xs)
  pure (xs !! (idx - 1))


selectPhone :: [TrustedPhone] -> IO TrustedPhone
selectPhone xs = do
  let putPhoneChoice (i, x) = Text.putStrLn $ "" +| i |+ ") " +| tpnNumberWithDialCode x |+ ""
  when (null xs) $ fail "sorry, expected to pick a trusted phone number, none to choose from"
  Text.putStrLn "Please select a trusted phone number to send a code to"
  mapM_ putPhoneChoice $ zip ([1 ..] :: [Int]) xs
  idx <- pleaseChooseN 1 (length xs)
  pure (xs !! (idx - 1))


pleaseChooseN :: Int -> Int -> IO Int
pleaseChooseN low high = do
  let readN = readMaybe <$> promptNonEmpty prefix
      prefix = "Please choose an option between " +| low |+ " and " +| high |+ ""
  readN >>= \case
    Nothing -> pleaseChooseN low high
    Just x | x < low || x > high -> pleaseChooseN low high
    Just x -> pure x


pleaseReadCode :: IO Text
pleaseReadCode = do
  let prefix = "Please enter the n-digit code you just received"
  Text.pack <$> promptNonEmpty prefix


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


-- | Selects a phone/device and applies the appropriate handler
withSelectedPhoneOrDevice
  :: (TrustedPhone -> IO a) -> (TrustedDevice -> IO a) -> TrustData -> IO a
withSelectedPhoneOrDevice handlePhone handleDevice = do
  let ikou (TrustedDevices ys) = selectDevice ys >>= handleDevice
      ikou (TrustedPhoneNumbers [y]) = handlePhone y
      ikou (TrustedPhoneNumbers ys) = selectPhone ys >>= handlePhone
  ikou . tdList


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
      noTrustedDevices = o .: "noTrustedDevices"
      isListKey key _ignored = key == "trustedPhoneNumbers" || key == "trustedDevices"
      theList = parseJSON (Object $ filterWithKey isListKey o)
   in TrustData <$> theList <*> securityCode <*> noTrustedDevices


instance ToJSON TrustData where
  toJSON = toJSONTrustData


instance FromJSON TrustData where
  parseJSON = parseJSONTrustData


simpleOptions :: Options
simpleOptions = aesonPrefix camelCase
