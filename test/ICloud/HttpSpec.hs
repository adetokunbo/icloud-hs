module ICloud.HttpSpec
  ( spec
  )
where

import Data.ByteString (ByteString)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified ICloud.Examples as Examples
import Network.HTTP.Types (HeaderName)
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
import Test.Hspec (Spec, context, describe, it)
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
