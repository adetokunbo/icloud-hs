{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : HStratus.TrustSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module HStratus.TrustSpec (spec, encode, genTrustData, genTrustedList, jsonKeysOf) where

import Data.Aeson (Key, ToJSON (..), Value (..), decode, eitherDecodeFileStrict, encode)
import Data.Aeson.KeyMap (fromList)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified HStratus.Examples as Examples
import Network.HStratus.Internal.Trust
import Paths_hstratus_auth (getDataFileName)
import System.IO.Silently (silence)
import Test.Hspec
  ( Spec
  , context
  , describe
  , it
  , shouldBe
  )
import Test.Hspec.Benri (endsJust, endsNothing, endsRight)
import Test.Main (withStdin)
import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , Gen
  , Property
  , chooseInt
  , elements
  , forAll
  , frequency
  , listOf1
  , vectorOf
  )
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)


spec :: Spec
spec = describe "module Network.HStratus.Trust" $ do
  describe "TrustData" $ do
    context "parsing generated examples to/from JSON" $ do
      it "should succeed" prop_jsonRoundtripTrustData
    context "parsing a hand-crafted Apple-shaped fixture" $ do
      it "should succeed" $ do
        fp <- getDataFileName "testdata/trust_data_test.json"
        eitherDecodeFileStrict fp `endsRight` expectedTrustData
  describe "CodeStatus JSON field names" $ do
    it "uses the server field names" $
      jsonKeysOf (CodeStatus 6 False False False False)
        `shouldBe` Just (sort ["length", "tooManyCodesSent", "tooManyCodesValidated", "securityCodeLocked", "securityCodeCooldown"])
  describe "CodeStatus JSON parsing" $ do
    it "defaults all boolean fields to False when absent" $
      decode "{\"length\":6}" `shouldBe` Just (CodeStatus 6 False False False False)
  describe "TrustedPhone JSON field names" $ do
    it "uses the server field names" $
      jsonKeysOf (TrustedPhone 1 "+81 test" (Just "sms"))
        `shouldBe` Just (sort ["id", "numberWithDialCode", "pushMode"])
  describe "TrustedDevice JSON field names" $ do
    it "uses the server field names" $
      jsonKeysOf (TrustedDevice "id1" "iPhone" "iPhone14")
        `shouldBe` Just (sort ["id", "name", "modelName"])
  describe "TrustedDevice JSON parsing" $ do
    it "defaults modelName to empty string when absent" $
      decode "{\"id\":\"1\",\"name\":\"iPhone\"}" `shouldBe` Just (TrustedDevice "1" "iPhone" "")

  describe "Setup2SADevice" $ do
    context "parsing generated examples to/from JSON" $ do
      it "should succeed" prop_jsonRoundtripSetup2SADevice

  describe "setup2SADeviceLabel" $ do
    it "returns phoneNumber when present" $
      setup2SADeviceLabel (mkDevice [("phoneNumber", String "+1234")]) `shouldBe` "+1234"
    it "returns name when phoneNumber is absent" $
      setup2SADeviceLabel (mkDevice [("name", String "iPhone")]) `shouldBe` "iPhone"
    it "prefers phoneNumber over name" $
      setup2SADeviceLabel (mkDevice [("phoneNumber", String "+1234"), ("name", String "iPhone")]) `shouldBe` "+1234"
    it "returns (unknown) when neither field is present" $
      setup2SADeviceLabel (mkDevice [("deviceId", String "abc")]) `shouldBe` "(unknown)"

  describe "selectPhone" $ do
    context "when the selected input" $ do
      context "is a number within the range" $ do
        it "should succeed" (prop_selectsWithNonMaxIndex selectPhone genTrustedPhone)
      context "is the maximum number" $ do
        it "should succeed" (prop_selectsWithMaxIndex selectPhone genTrustedPhone)

  describe "selectDevice" $ do
    context "when the selected input" $ do
      context "is a number within the range" $ do
        it "should succeed" (prop_selectsWithNonMaxIndex selectDevice genTrustedDevice)
      context "is the maximum number" $ do
        it "should succeed" (prop_selectsWithMaxIndex selectDevice genTrustedDevice)

  describe "selectSetupDevice" $ do
    context "when the selected input" $ do
      context "is a number within the range" $ do
        it "should succeed" (prop_selectsWithNonMaxIndex selectSetupDevice genSetup2SADevice)
      context "is the maximum number" $ do
        it "should succeed" (prop_selectsWithMaxIndex selectSetupDevice genSetup2SADevice)

  describe "selectTwoFaPhone" $ do
    context "when noTrustedDevices is True" $ do
      it "returns the first phone without prompting" $
        silence (selectTwoFaPhone (mkTrustData True [twoFaPhone1, twoFaPhone2]))
          `endsJust` twoFaPhone1
      it "returns Nothing when the phone list is empty" $
        endsNothing $
          silence (selectTwoFaPhone (mkTrustData True []))
    context "when noTrustedDevices is False" $ do
      context "and the phone list is empty" $ do
        it "returns Nothing without prompting" $
          endsNothing $
            silence (selectTwoFaPhone (mkTrustData False []))
      context "and the user presses Enter" $ do
        it "returns Nothing" $
          withStdin "\n" $
            endsNothing $
              silence (selectTwoFaPhone (mkTrustData False [twoFaPhone1]))
      context "and the user enters a valid index" $ do
        it "returns the selected phone" $
          withStdin "2" $
            silence (selectTwoFaPhone (mkTrustData False [twoFaPhone1, twoFaPhone2]))
              `endsJust` twoFaPhone2
      context "and the user first enters an invalid index" $ do
        it "retries and returns the selected phone" $
          withStdin (toS ("99\n1" :: String)) $
            silence (selectTwoFaPhone (mkTrustData False [twoFaPhone1, twoFaPhone2]))
              `endsJust` twoFaPhone1


prop_jsonRoundtripTrustData :: Property
prop_jsonRoundtripTrustData = forAll genTrustData $ \td ->
  decode (encode td) == Just td


prop_jsonRoundtripSetup2SADevice :: Property
prop_jsonRoundtripSetup2SADevice = forAll genSetup2SADevice $ \d ->
  decode (encode d) == Just d


mkDevice :: [(Key, Value)] -> Setup2SADevice
mkDevice = Setup2SADevice . fromList


genSetup2SADevice :: Gen Setup2SADevice
genSetup2SADevice = do
  phone <- genExWordMaybe
  devId <- genExWord
  let pairs =
        catMaybes
          [ Just ("deviceId", String devId)
          , fmap (\p -> ("phoneNumber", String p)) phone
          ]
  pure $ Setup2SADevice $ fromList pairs


genCodeStatus :: Gen CodeStatus
genCodeStatus =
  CodeStatus
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


genTrustedPhone :: Gen TrustedPhone
genTrustedPhone = TrustedPhone <$> arbitrary <*> genExWord <*> genExWordMaybe


genTrustedDevice :: Gen TrustedDevice
genTrustedDevice = TrustedDevice <$> genExWord <*> genExWord <*> genExWord


genTrustedList :: Gen TrustedList
genTrustedList =
  frequency
    [ (1, TrustedPhoneNumbers . NE.fromList <$> listOf1 genTrustedPhone)
    , (1, TrustedDevices . NE.fromList <$> listOf1 genTrustedDevice)
    ]


twoFaPhone1 :: TrustedPhone
twoFaPhone1 = TrustedPhone 1 "+81 test-1" (Just "sms")


twoFaPhone2 :: TrustedPhone
twoFaPhone2 = TrustedPhone 2 "+1 test-2" Nothing


mkTrustData :: Bool -> [TrustedPhone] -> TrustData
mkTrustData noDevices phones =
  TrustData
    { tdList = case NE.nonEmpty phones of
        Just nep -> TrustedPhoneNumbers nep
        Nothing -> TrustedDevices (TrustedDevice "" "" "" :| [])
    , tdSecurityCode = CodeStatus 6 False False False False
    , tdNoTrustedDevices = noDevices
    }


useIOSelector :: (Eq a) => (NonEmpty a -> IO a) -> (Int, a, NonEmpty a) -> IO Bool
useIOSelector selector (idx, want, xs) = do
  withStdin (toS $ show idx) $ do
    selected <- silence $ selector xs
    pure $ selected == want


prop_selectsWithNonMaxIndex :: (Eq a, Show a) => (NonEmpty a -> IO a) -> Gen a -> Property
prop_selectsWithNonMaxIndex selector generator = monadicIO $ do
  pick (genWithNonMaxIndex generator) >>= run . useIOSelector selector >>= assert


prop_selectsWithMaxIndex :: (Eq a, Show a) => (NonEmpty a -> IO a) -> Gen a -> Property
prop_selectsWithMaxIndex selector generator = monadicIO $ do
  let useMax (_ignoredIndex, _ignoredSelection, xs) = (NE.length xs, NE.last xs, xs)
      withNonMax = genWithNonMaxIndex generator
  pick (fmap useMax withNonMax) >>= run . useIOSelector selector >>= assert


genWithNonMaxIndex :: Gen a -> Gen (Int, a, NonEmpty a)
genWithNonMaxIndex sourceGen = do
  low <- chooseInt (1, 5)
  high <- chooseInt (low, 10)
  xs <- NE.fromList <$> vectorOf high sourceGen -- vectorOf high always gives exactly high elements, high >= 1
  pure (low, NE.toList xs !! (low - 1), xs) -- low in [1..high], so in-bounds


genTrustData :: Gen TrustData
genTrustData = TrustData <$> genTrustedList <*> genCodeStatus <*> arbitrary


genExWord :: Gen Text
genExWord = elements Examples.wordz


genExWordMaybe :: Gen (Maybe Text)
genExWordMaybe = frequency [(1, pure Nothing), (1, Just <$> genExWord)]


jsonKeysOf :: (ToJSON a) => a -> Maybe [Key]
jsonKeysOf x = case toJSON x of
  Object o -> Just (sort $ KeyMap.keys o)
  _ -> Nothing


expectedTrustData :: TrustData
expectedTrustData =
  TrustData
    { tdList = TrustedPhoneNumbers (TrustedPhone 1 "+81 \x2022\x2022 \x2022\x2022\x2022\x2022 \x2022\&34" (Just "sms") :| [])
    , tdSecurityCode = CodeStatus 6 False False False False
    , tdNoTrustedDevices = False
    }
