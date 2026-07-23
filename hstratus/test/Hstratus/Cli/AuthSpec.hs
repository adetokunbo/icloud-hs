module Hstratus.Cli.AuthSpec (spec) where

import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Auth (AuthCommand (..), LoginOpts (..))
import Options.Applicative
  ( ParserResult (..)
  , defaultPrefs
  , execParserPure
  , renderFailure
  )
import Test.Hspec


parseCmd :: [String] -> Either String TopCommand
parseCmd args =
  case execParserPure defaultPrefs cliParser args of
    Success cmd -> Right cmd
    Failure failure -> Left (fst (renderFailure failure "test"))
    CompletionInvoked _ -> Left "completion invoked"


spec :: Spec
spec = describe "auth parser" $ do
  it "parses auth init" $
    parseCmd ["auth", "init"] `shouldBe` Right (AuthCmd AuthInit)

  it "parses auth login --china" $ do
    case parseCmd ["auth", "login", "--china"] of
      Right (AuthCmd (AuthLogin opts)) -> loginChina opts `shouldBe` True
      other -> expectationFailure $ "unexpected result: " <> show other

  it "parses auth login --log-file FILE" $ do
    case parseCmd ["auth", "login", "--log-file", "/tmp/x"] of
      Right (AuthCmd (AuthLogin opts)) -> loginLogFile opts `shouldBe` Just "/tmp/x"
      other -> expectationFailure $ "unexpected result: " <> show other

  it "parses auth login --redact" $ do
    case parseCmd ["auth", "login", "--redact"] of
      Right (AuthCmd (AuthLogin opts)) -> loginRedact opts `shouldBe` True
      other -> expectationFailure $ "unexpected result: " <> show other
