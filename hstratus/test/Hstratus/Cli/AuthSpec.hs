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
import Test.Hspec.Benri (endsRight)


parseCmd :: [String] -> IO (Either String TopCommand)
parseCmd args =
  pure $ case execParserPure defaultPrefs cliParser args of
    Success cmd -> Right cmd
    Failure failure -> Left (fst (renderFailure failure "test"))
    CompletionInvoked _ -> Left "completion invoked"


defaultLoginOpts :: LoginOpts
defaultLoginOpts = LoginOpts False False Nothing False


spec :: Spec
spec = describe "auth parser" $ do
  it "parses auth init" $
    parseCmd ["auth", "init"]
      `endsRight` AuthCmd AuthInit

  it "parses auth login --china" $
    parseCmd ["auth", "login", "--china"]
      `endsRight` AuthCmd (AuthLogin defaultLoginOpts{loginChina = True})

  it "parses auth login --log-file FILE" $
    parseCmd ["auth", "login", "--log-file", "/tmp/x"]
      `endsRight` AuthCmd (AuthLogin defaultLoginOpts{loginLogFile = Just "/tmp/x"})

  it "parses auth login --redact" $
    parseCmd ["auth", "login", "--redact"]
      `endsRight` AuthCmd (AuthLogin defaultLoginOpts{loginRedact = True})
