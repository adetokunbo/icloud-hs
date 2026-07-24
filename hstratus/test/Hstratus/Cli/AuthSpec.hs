module Hstratus.Cli.AuthSpec (spec) where

import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Auth (AuthCommand (..))
import Network.HStratus.Http.Cli (CommonOpts (..))
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


defaultOpts :: CommonOpts
defaultOpts = CommonOpts False False Nothing False False


spec :: Spec
spec = describe "auth parser" $ do
  it "parses auth init" $
    parseCmd ["auth", "init"]
      `endsRight` AuthCmd AuthInit

  it "parses auth login --china" $
    parseCmd ["auth", "login", "--china"]
      `endsRight` AuthCmd (AuthLogin defaultOpts{optChina = True})

  it "parses auth login --log-file FILE" $
    parseCmd ["auth", "login", "--log-file", "/tmp/x"]
      `endsRight` AuthCmd (AuthLogin defaultOpts{optLogFile = Just "/tmp/x"})

  it "parses auth login --log-bodies" $
    parseCmd ["auth", "login", "--log-bodies"]
      `endsRight` AuthCmd (AuthLogin defaultOpts{optLogBodies = True})

  it "parses auth login --redact" $
    parseCmd ["auth", "login", "--redact"]
      `endsRight` AuthCmd (AuthLogin defaultOpts{optRedact = True})
