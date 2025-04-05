{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ICloud.KDFSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module ICloud.KDFSpec (spec) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import Data.Aeson (
  FromJSON (..),
  Object,
  Value,
  eitherDecodeFileStrict,
  withArray,
  withObject,
  (.:),
 )
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decode)
import Data.Foldable (toList)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32, Word64, Word8)
import Network.ICloud.KDF (PseudoRandomF, calcPBKDF2, wrap)
import Paths_icloud_auth (getDataFileName)
import Test.Hspec (Spec, context, describe, it, runIO, shouldBe)
import Test.Hspec.Runner (SpecWith)


spec :: Spec
spec = describe "module Network.ICloud.KDF" $ do
  describe "calcPBKDF2" $ do
    context "when using the wycheProof test cases" $ do
      specFrom "sha1" SHA1.hmac
      specFrom "sha256" SHA256.hmac
      specFrom "sha512" SHA512.hmac


specFrom :: String -> PseudoRandomF -> Spec
specFrom shaName pseudo = do
  context ("with " ++ shaName ++ ".hmac as the pseudorandom function") $ do
    testData <- runIO $ namedDataPath shaName >>= loadTestData
    mapM_ (specWithFrom pseudo) testData


specWithFrom :: PseudoRandomF -> TestDescription -> SpecWith ()
specWithFrom pseudo td = do
  let TestDescription {tdSalt, tdIterationCount = count, tdDerivedKey = key} = td
  it ("test " ++ show (tdId td) ++ " should succeed") $ do
    let pseudo' = wrap pseudo (tdDerivedLength td)
        calc x = calcPBKDF2 x (tdPassword td) tdSalt count
    calc <$> pseudo' `shouldBe` Right key


loadTestData :: FilePath -> IO [TestDescription]
loadTestData src =
  let decodeDataFile aPath = fmap unTestFile <$> eitherDecodeFileStrict aPath
   in decodeDataFile src >>= either fail pure


namedDataPath :: String -> IO FilePath
namedDataPath shaName =
  let path = "testdata/pbkdf2_hmac" ++ shaName ++ "_test.json"
   in getDataFileName path


newtype TestFile = TestFile {unTestFile :: [TestDescription]}


instance FromJSON TestFile where
  parseJSON = fmap TestFile . parseTestFile


parseTestFile :: Value -> Parser [TestDescription]
parseTestFile =
  let oneTest = withObject "test" parseTestDescription
      manyTests = traverse oneTest . toList
      testsInTestGroup o = o .: "tests" >>= withArray "[test]" manyTests
      oneTestGroup = withObject "testsInTestGroup" testsInTestGroup
      manyTestGroups = traverse oneTestGroup . toList
      testGroupsAtTop o = o .: "testGroups" >>= withArray "[testGroups]" manyTestGroups
   in withObject "dataFile" (fmap concat . testGroupsAtTop)


data TestDescription = TestDescription
  { tdId :: !Word8
  , tdPassword :: !ByteString
  , tdSalt :: !ByteString
  , tdIterationCount :: !Word64
  , tdDerivedLength :: !Word32
  , tdDerivedKey :: !ByteString
  }
  deriving (Eq, Show)


parseTestDescription :: Object -> Parser TestDescription
parseTestDescription o = do
  let parseBase16Bytes = either fail pure . decode . encodeUtf8
  tdId <- o .: "tcId"
  tdPassword <- o .: "password" >>= parseBase16Bytes
  tdSalt <- o .: "salt" >>= parseBase16Bytes
  tdIterationCount <- o .: "iterationCount"
  tdDerivedLength <- o .: "dkLen"
  tdDerivedKey <- o .: "dk" >>= parseBase16Bytes
  pure
    TestDescription
      { tdId
      , tdPassword
      , tdSalt
      , tdIterationCount
      , tdDerivedKey
      , tdDerivedLength
      }
