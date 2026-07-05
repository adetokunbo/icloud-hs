{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ICloud.TrustSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module ICloud.TrustSpec (spec, encode, genTrustData, genTrustedList) where

import Data.Aeson (Key, Value (..), decode, encode)
import Data.Aeson.KeyMap (fromList)
import Data.Maybe (catMaybes)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified ICloud.Examples as Examples
import Network.ICloud.Trust
import System.IO.Silently (silence)
import Test.Hspec
  ( Spec
  , context
  , describe
  , it
  , shouldBe
  )
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
spec = describe "module Network.ICloud.Trust" $ do
  describe "TrustData" $ do
    context "parsing generated examples to/from JSON" $ do
      it "should succeed" prop_jsonRoundtripTrustData

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
    [ (1, TrustedPhoneNumbers <$> listOf1 genTrustedPhone)
    , (1, TrustedDevices <$> listOf1 genTrustedDevice)
    ]


useIOSelector :: (Eq a) => ([a] -> IO a) -> (Int, a, [a]) -> IO Bool
useIOSelector selector (idx, want, xs) = do
  withStdin (toS $ show idx) $ do
    selected <- silence $ selector xs
    pure $ selected == want


prop_selectsWithNonMaxIndex :: (Eq a, Show a) => ([a] -> IO a) -> Gen a -> Property
prop_selectsWithNonMaxIndex selector generator = monadicIO $ do
  pick (genWithNonMaxIndex generator) >>= run . useIOSelector selector >>= assert


prop_selectsWithMaxIndex :: (Eq a, Show a) => ([a] -> IO a) -> Gen a -> Property
prop_selectsWithMaxIndex selector generator = monadicIO $ do
  let useMax (_ignoredIndex, _ignoredSelection, xs) =
        let num = length xs
         in (num, xs !! (num - 1), xs)
      withNonMax = genWithNonMaxIndex generator
  pick (fmap useMax withNonMax) >>= run . useIOSelector selector >>= assert


genWithNonMaxIndex :: Gen a -> Gen (Int, a, [a])
genWithNonMaxIndex sourceGen = do
  low <- chooseInt (1, 5)
  high <- chooseInt (low, 10)
  xs <- vectorOf high sourceGen
  pure (low, xs !! (low - 1), xs)


genTrustData :: Gen TrustData
genTrustData = TrustData <$> genTrustedList <*> genCodeStatus <*> arbitrary


genExWord :: Gen Text
genExWord = elements Examples.wordz


genExWordMaybe :: Gen (Maybe Text)
genExWordMaybe = frequency [(1, pure Nothing), (1, Just <$> genExWord)]
