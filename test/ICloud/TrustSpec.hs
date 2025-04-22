{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ICloud.TrustSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module ICloud.TrustSpec (spec, encode, genTrustData, genTrustedList) where

import Data.Aeson (decode, encode)
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


prop_jsonRoundtripTrustData :: Property
prop_jsonRoundtripTrustData = forAll genTrustData $ \td ->
  decode (encode td) == Just td


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
