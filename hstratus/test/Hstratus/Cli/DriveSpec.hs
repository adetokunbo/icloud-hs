{-# LANGUAGE OverloadedStrings #-}

module Hstratus.Cli.DriveSpec (spec) where

import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Drive (DriveCommand (..), ListFolderOpts (..))
import Network.HStratus.Http.Cli (CommonOpts (..))
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
spec = describe "drive parser" $ do
  it "parses drive list-root" $ do
    case parseCmd ["drive", "list-root"] of
      Right (DriveCmd (DriveListRoot _)) -> pure ()
      other -> expectationFailure $ "unexpected result: " <> show other

  it "parses drive list-folder PATH" $ do
    case parseCmd ["drive", "list-folder", "Documents/Work"] of
      Right (DriveCmd (DriveListFolder opts)) ->
        lfPath opts `shouldBe` ["Documents", "Work"]
      other -> expectationFailure $ "unexpected result: " <> show other

  it "parses drive list-root --china --log" $ do
    case parseCmd ["drive", "list-root", "--china", "--log"] of
      Right (DriveCmd (DriveListRoot opts)) -> do
        optChina opts `shouldBe` True
        optLog opts `shouldBe` True
      other -> expectationFailure $ "unexpected result: " <> show other
