{-# LANGUAGE OverloadedStrings #-}

module HStratus.Http.CliSpec (spec) where

import Network.HStratus.Http.Cli (CommonOpts (..), commonOptsParser)
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, fullDesc, info, renderFailure)
import Test.Hspec


spec :: Spec
spec = describe "Network.HStratus.Http.Cli.commonOptsParser" $ do
  it "defaults all flags to False with no log file" $ do
    case parseOpts [] of
      Right opts -> do
        optChina opts `shouldBe` False
        optLog opts `shouldBe` False
        optLogFile opts `shouldBe` Nothing
        optLogBodies opts `shouldBe` False
        optRedact opts `shouldBe` False
      Left err -> expectationFailure err

  it "sets optChina when --china is given" $
    fmap optChina (parseOpts ["--china"]) `shouldBe` Right True

  it "sets optLog when --log is given" $
    fmap optLog (parseOpts ["--log"]) `shouldBe` Right True

  it "sets optLogFile when --log-file is given" $
    fmap optLogFile (parseOpts ["--log-file", "/tmp/test.log"])
      `shouldBe` Right (Just "/tmp/test.log")

  it "sets optLogBodies when --log-bodies is given" $
    fmap optLogBodies (parseOpts ["--log-bodies"]) `shouldBe` Right True

  it "sets optRedact when --redact is given" $
    fmap optRedact (parseOpts ["--redact"]) `shouldBe` Right True

  it "accepts multiple flags together" $
    case parseOpts ["--log", "--redact", "--china"] of
      Right opts -> do
        optLog opts `shouldBe` True
        optRedact opts `shouldBe` True
        optChina opts `shouldBe` True
      Left err -> expectationFailure err


parseOpts :: [String] -> Either String CommonOpts
parseOpts args =
  case execParserPure defaultPrefs (info commonOptsParser fullDesc) args of
    Success opts -> Right opts
    Failure failure -> Left (fst (renderFailure failure "test"))
    CompletionInvoked _ -> Left "completion invoked"
