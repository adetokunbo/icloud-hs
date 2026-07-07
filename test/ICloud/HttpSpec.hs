{-# LANGUAGE OverloadedStrings #-}

module ICloud.HttpSpec
  ( spec
  )
where

import Crypto.SRP.Hashing (KnownAlgorithm (SHA256), hashText)
import Data.Aeson (Value (..), decode, withObject, (.:))
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified ICloud.Examples as Examples
import Network.HTTP.Types (HeaderName)
import Network.ICloud.Http.Internal (validateSetupBody)
import Network.ICloud.Session
  ( PasswordProtocol (..)
  , SavedHeaders (..)
  , hCounter
  , hCountry
  , hSessionId
  , hSessionToken
  , hTrustToken
  , pristine
  , updateSavedHeaders
  )
import Network.ICloud.Trust (Setup2SADevice (..))
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.QuickCheck
  ( Gen
  , Property
  , elements
  , forAll
  , forAllBlind
  , sublistOf
  , vectorOf
  )


spec :: Spec
spec = describe "module Network.ICloud.Http" $ do
  updateSavedHeadersSpec
  passwordProtocolSpec
  validateSetupBodySpec


updateSavedHeadersSpec :: Spec
updateSavedHeadersSpec = describe "updateSavedHeaders" $ do
  context "using generated headers" $ do
    it "should generated the expected value" prop_updateSavedHeaders


prop_updateSavedHeaders :: Property
prop_updateSavedHeaders = forAllBlind genHdrsAndExpectedSavedHeaders $ \(hdrs, f) ->
  f $ updateSavedHeaders hdrs pristine


genHdrsAndExpectedSavedHeaders :: Gen ([(HeaderName, ByteString)], SavedHeaders -> Bool)
genHdrsAndExpectedSavedHeaders = do
  checks <- sublistOf sdChecks
  values <- vectorOf (length checks) (elements Examples.byteStrings)
  let headers = zip (map fst checks) values
      asPred getter want sd = Just (toS want) == getter sd
      preds = zipWith asPred (map snd checks) values
      combine xs sd = all ($ sd) xs
  pure (headers, combine preds)


sdChecks :: [(HeaderName, SavedHeaders -> Maybe Text)]
sdChecks =
  [ (hCountry, shCountry)
  , (hSessionId, shSessionId)
  , (hSessionToken, shSessionToken)
  , (hTrustToken, shTrustToken)
  , (hCounter, shCounter)
  ]


passwordProtocolSpec :: Spec
passwordProtocolSpec = describe "PasswordProtocol" $ do
  context "parsing from JSON" $ do
    it "parses 's2k' as New" $
      (decode "\"s2k\"" :: Maybe PasswordProtocol) `shouldBe` Just New
    it "parses 's2k_fo' as Old" $
      (decode "\"s2k_fo\"" :: Maybe PasswordProtocol) `shouldBe` Just Old
    it "fails on unknown strings" $
      (decode "\"unknown\"" :: Maybe PasswordProtocol) `shouldBe` Nothing
  context "key derivation" $ do
    it "Old (Base16-encoded hash) always differs from New (raw hash)" $
      prop_oldNewHashesDiffer


prop_oldNewHashesDiffer :: Property
prop_oldNewHashesDiffer = forAll (elements Examples.wordz) $ \pwd ->
  let hashed = hashText SHA256 pwd
   in Base16.encode hashed /= hashed


validateSetupBodySpec :: Spec
validateSetupBodySpec = describe "validateSetupBody" $ do
  it "includes verificationCode from the code argument" $
    field "verificationCode" `shouldBe` Just (String "123456")
  it "includes trustBrowser set to True" $
    field "trustBrowser" `shouldBe` Just (Bool True)
  it "preserves original device fields" $
    field "deviceId" `shouldBe` Just (String "abc")
 where
  device = Setup2SADevice $ fromList [("deviceId", String "abc")]
  body = validateSetupBody device "123456"
  field k = parseMaybe (withObject "body" (.: k)) body
