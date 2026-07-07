{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

module Network.ICloud.Internal.Http
  ( validateSetupBody
  , PasswordProtocol (..)
  , KeyDeriver (..)
  , SrpContext (..)
  , hCounter
  , hCountry
  , hSessionId
  , hSessionToken
  , hTrustToken
  )
where

import Crypto.SRP
  ( FromClient (..)
  , FromServer (..)
  , XCalculator (..)
  , hashMany
  , hashText
  )
import Data.Aeson (FromJSON (..), Value (..), withText)
import Data.Aeson.KeyMap (fromList)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.CaseInsensitive (mk)
import Data.Text (Text)
import Data.Word (Word64)
import Network.HTTP.Types.Header (HeaderName)
import Network.ICloud.Internal.PBKDF2 (FancyPseudoRandomF, deriveKey)
import Network.ICloud.Internal.Trust (Setup2SADevice (..))


-- | Models the known values of password protocol
data PasswordProtocol = Old | New
  deriving (Eq, Show)


instance FromJSON PasswordProtocol where
  parseJSON =
    let fromText "s2k" = Right New
        fromText "s2k_fo" = Right Old
        fromText alt = Left $ "unknown PasswordProtocol: " ++ show alt
     in withText "PasswordProtocol" $ either fail pure . fromText


-- | Data used during key derivation and verification
data KeyDeriver = KeyDeriver
  { kdTag :: !Text
  , kdIterations :: !Word64
  , kdProtocol :: !PasswordProtocol
  , kdWrappedF :: !FancyPseudoRandomF
  }


instance XCalculator KeyDeriver where
  calcX = calcXUsingKeyDeriver


calcXUsingKeyDeriver :: KeyDeriver -> FromClient -> FromServer -> BS.ByteString
calcXUsingKeyDeriver kd fc fs =
  let FromServer{fsSalt, fsKnownAlgorithm = hashAlgo} = fs
      h = hashMany hashAlgo
      KeyDeriver{kdIterations = count, kdWrappedF, kdProtocol} = kd
      useProtocol Old = Base16.encode
      useProtocol New = id
      hashed = useProtocol kdProtocol $ hashText hashAlgo $ fcPassword fc
      reallyHashed = deriveKey kdWrappedF hashed fsSalt count
   in h [fsSalt, h [":", reallyHashed]]


-- | Bundles the SRP client\/server data and key deriver for a single auth attempt
data SrpContext = SrpContext
  { srpFromClient :: !FromClient
  , srpFromServer :: !FromServer
  , srpKeyDeriver :: !KeyDeriver
  }


-- | @HeaderName@s used to capture session info from HTTP responses
hCountry, hSessionId, hSessionToken, hTrustToken, hCounter :: HeaderName
hCountry = mk "X-Apple-ID-Account-Country"
hSessionId = mk "X-Apple-ID-Session-Id"
hSessionToken = mk "X-Apple-Session-Token"
hTrustToken = mk "X-Apple-TwoSV-Trust-Token"
hCounter = mk "scnt"


validateSetupBody :: Setup2SADevice -> Text -> Value
validateSetupBody (Setup2SADevice fields) code =
  Object $ fields <> fromList [("verificationCode", String code), ("trustBrowser", Bool True)]
