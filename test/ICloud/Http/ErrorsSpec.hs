{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ICloud.Http.ErrorsSpec
  ( spec
  )
where

import Control.Exception (throwIO, try)
import Data.Aeson (Value (..), decode, encode, object)
import Data.Aeson.Key (fromText)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified ICloud.Examples as Examples
import Network.ICloud.Internal.HttpErrors
  ( ApiError (..)
  , ApiResponse (..)
  , AuthError (..)
  , extractOr
  )
import Test.Hspec (Spec, context, describe, it, shouldReturn)
import Test.QuickCheck
  ( Gen
  , Property
  , elements
  , forAll
  , frequency
  )


spec :: Spec
spec = describe "module Network.ICloud.Http.Error" $ do
  apiErrorSpec
  authErrorSpec


apiErrorSpec :: Spec
apiErrorSpec = describe "ApiError" $ do
  context "parsing it from JSON" $ do
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


authErrorSpec :: Spec
authErrorSpec = describe "AuthError" $ do
  context "is catchable with try @AuthError" $ do
    it "catches InvalidCredentials" $
      catchAuthError InvalidCredentials `shouldReturn` Left InvalidCredentials
    it "catches AccountLocked" $
      catchAuthError AccountLocked `shouldReturn` Left AccountLocked
    it "catches PrivacyAgreementRequired" $
      catchAuthError PrivacyAgreementRequired `shouldReturn` Left PrivacyAgreementRequired
    it "catches ServiceError" $
      catchAuthError (ServiceError "reason" (Just "code")) `shouldReturn` Left (ServiceError "reason" (Just "code"))
    it "catches UnexpectedResponse" $
      catchAuthError (UnexpectedResponse "oops") `shouldReturn` Left (UnexpectedResponse "oops")

  context "extractOr on a Failed ApiResponse" $ do
    it "throws ServiceError with the ApiError reason and code" $
      catchAuthError' (extractOr (Failed (ApiError "bad" (Just "E1"))))
        `shouldReturn` Right (ServiceError "bad" (Just "E1"))


catchAuthError :: AuthError -> IO (Either AuthError AuthError)
catchAuthError e = try (throwIO e)


catchAuthError' :: IO a -> IO (Either AuthError AuthError)
catchAuthError' action =
  try action >>= \case
    Left e -> pure (Right e)
    Right _ -> pure (Left (UnexpectedResponse "expected AuthError but got success"))
