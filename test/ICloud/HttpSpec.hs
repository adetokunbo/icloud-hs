module ICloud.HttpSpec (
  spec,
) where

import Data.Aeson (Value (..), decode, encode, object)
import Data.Aeson.Key (fromText)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified ICloud.Examples as Examples
import Network.HTTP.Types (HeaderName)
import Network.ICloud.Auth (SessionData (..))
import Network.ICloud.Http (
  ApiError (..),
  hCounter,
  hCountry,
  hSessionId,
  hSessionToken,
  hTrustToken,
  mkSessionData,
 )
import Test.Hspec (Spec, context, describe, it)
import Test.QuickCheck (
  Gen,
  Property,
  elements,
  forAll,
  forAllBlind,
  frequency,
  sublistOf,
  vectorOf,
 )


spec :: Spec
spec = describe "module Network.ICloud.Http" $ do
  httpSpec
  sessionDataSpec


sessionDataSpec :: Spec
sessionDataSpec = describe "mkSessionData" $ do
  context "using generated headers" $ do
    it "should generated the expected value" prop_mkSessionData


httpSpec :: Spec
httpSpec = describe "ApiError" $ do
  context "when parsing from JSON" $ do
    it "should succeed" prop_parseJSONApiError


prop_parseJSONApiError :: Property
prop_parseJSONApiError = forAll genApiErrorWithJsonEncoding $ \(encoded, ae) ->
  decode (BS.fromStrict encoded) == Just ae


genApiErrorWithJsonEncoding :: Gen (ByteString, ApiError)
genApiErrorWithJsonEncoding = do
  reasonKV <- genKeyValue $ elements Examples.errorKeys
  mbCodeKV <- genKeyValueMb $ elements Examples.codeKeys
  let ae =
        ApiError
          { aeReason = snd reasonKV
          , aeCode = snd <$> mbCodeKV
          }
      asKV (x, y) = (fromText x, String y)
      objectParts = catMaybes [Just reasonKV, mbCodeKV]
      encoded = BS.toStrict $ encode $ object $ map asKV objectParts
  pure (encoded, ae)


{- |
generate a value or Nothing as the value of field
when there is a value, generate the value of the key
-}
genKeyValueMb :: Gen Text -> Gen (Maybe (Text, Text))
genKeyValueMb keyGen = do
  valueMb <-
    frequency
      [ (1, Just <$> elements Examples.wordz)
      , (2, pure Nothing)
      ]
  case valueMb of
    Nothing -> pure Nothing
    Just x -> do
      key <- keyGen
      pure (Just (key, x))


genKeyValue :: Gen Text -> Gen (Text, Text)
genKeyValue keyGen = do
  value <- elements Examples.wordz
  key <- keyGen
  pure (key, value)


prop_mkSessionData :: Property
prop_mkSessionData = forAllBlind genHdrsAndExpectedSessionData $ \(hdrs, f) ->
  f $ mkSessionData hdrs


genHdrsAndExpectedSessionData :: Gen ([(HeaderName, ByteString)], SessionData -> Bool)
genHdrsAndExpectedSessionData = do
  checks <- sublistOf sdChecks
  values <- vectorOf (length checks) (elements Examples.byteStrings)
  let headers = zip (map fst checks) values
      asPred getter want sd = Just (toS want) == getter sd
      preds = zipWith asPred (map snd checks) values
      combine xs sd = all ($ sd) xs
  pure (headers, combine preds)


sdChecks :: [(HeaderName, SessionData -> Maybe Text)]
sdChecks =
  [ (hCountry, sdAccountCountry)
  , (hSessionId, sdSessionId)
  , (hSessionToken, sdSessionToken)
  , (hTrustToken, sdTrustToken)
  , (hCounter, sdCounter)
  ]
