{-# LANGUAGE OverloadedStrings #-}

module ICloud.HttpSpec
  ( spec
  )
where

import Data.Aeson (Value (..), withObject, (.:))
import Data.Aeson.KeyMap (fromList)
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified ICloud.Examples as Examples
import Network.HTTP.Types (HeaderName)
import Network.ICloud.Http (AsVerifyRequest (..), validateSetupBody)
import Network.ICloud.Session
  ( SavedHeaders (..)
  , hCounter
  , hCountry
  , hSessionId
  , hSessionToken
  , hTrustToken
  , pristine
  , updateSavedHeaders
  )
import Network.ICloud.Trust (Setup2SADevice (..), TrustedDevice (..), TrustedPhone (..))
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.QuickCheck
  ( Gen
  , Property
  , elements
  , forAllBlind
  , sublistOf
  , vectorOf
  )


spec :: Spec
spec = describe "module Network.ICloud.Http" $ do
  updateSavedHeadersSpec
  verifyCodeTypeSpec
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


verifyCodeTypeSpec :: Spec
verifyCodeTypeSpec = describe "verifyCodeType" $ do
  let phone  = TrustedPhone 1 "+1234" Nothing
      device = TrustedDevice "dev-id" "MacBook" "Mac"
  it "is 'phone' for TrustedPhone" $
    verifyCodeType phone `shouldBe` "phone"
  it "is 'trusteddevice' for TrustedDevice" $
    verifyCodeType device `shouldBe` "trusteddevice"



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
