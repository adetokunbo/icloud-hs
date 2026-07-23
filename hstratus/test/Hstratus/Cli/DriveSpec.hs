{-# LANGUAGE OverloadedStrings #-}

module Hstratus.Cli.DriveSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Drive (CpOpts (..), DriveCommand (..), ListFolderOpts (..))
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

  it "parses drive cp PATH with no dest option" $ do
    case parseCmd ["drive", "cp", "Documents/report.pdf"] of
      Right (DriveCmd (DriveCp opts)) -> do
        cpSrcPath opts `shouldBe` ("Documents" :| ["report.pdf"])
        cpRoot opts `shouldBe` Nothing
        cpOutput opts `shouldBe` Nothing
      other -> expectationFailure $ "unexpected result: " <> show other

  it "parses drive cp PATH --root DIR" $ do
    case parseCmd ["drive", "cp", "Documents/Work/report.pdf", "--root", "/tmp/dl"] of
      Right (DriveCmd (DriveCp opts)) -> do
        cpSrcPath opts `shouldBe` ("Documents" :| ["Work", "report.pdf"])
        cpRoot opts `shouldBe` Just "/tmp/dl"
      other -> expectationFailure $ "unexpected result: " <> show other

  it "parses drive cp PATH --output FILE" $ do
    case parseCmd ["drive", "cp", "Documents/report.pdf", "--output", "/tmp/report.pdf"] of
      Right (DriveCmd (DriveCp opts)) -> do
        cpSrcPath opts `shouldBe` ("Documents" :| ["report.pdf"])
        cpOutput opts `shouldBe` Just "/tmp/report.pdf"
      other -> expectationFailure $ "unexpected result: " <> show other

  it "parses drive cp single-segment PATH" $ do
    case parseCmd ["drive", "cp", "report.pdf"] of
      Right (DriveCmd (DriveCp opts)) -> do
        cpSrcPath opts `shouldBe` ("report.pdf" :| [])
        cpRoot opts `shouldBe` Nothing
        cpOutput opts `shouldBe` Nothing
      other -> expectationFailure $ "unexpected result: " <> show other

  it "parses drive cp PATH --root and --output together (conflict caught at runtime)" $ do
    case parseCmd ["drive", "cp", "Documents/report.pdf", "--root", "/tmp/dl", "--output", "/tmp/out.pdf"] of
      Right (DriveCmd (DriveCp opts)) -> do
        cpRoot opts `shouldBe` Just "/tmp/dl"
        cpOutput opts `shouldBe` Just "/tmp/out.pdf"
      other -> expectationFailure $ "unexpected result: " <> show other
