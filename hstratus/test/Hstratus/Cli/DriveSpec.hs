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
spec = describe "drive parser" $ do
  it "parses drive list-root" $
    parseCmd ["drive", "list-root"]
      `endsRight` DriveCmd (DriveListRoot defaultOpts)

  it "parses drive list-folder PATH" $
    parseCmd ["drive", "list-folder", "Documents/Work"]
      `endsRight` DriveCmd (DriveListFolder (ListFolderOpts ["Documents", "Work"] defaultOpts))

  it "parses drive list-root --china --log" $
    parseCmd ["drive", "list-root", "--china", "--log"]
      `endsRight` DriveCmd (DriveListRoot defaultOpts{optChina = True, optLog = True})

  it "parses drive cp PATH with no dest option" $
    parseCmd ["drive", "cp", "Documents/report.pdf"]
      `endsRight` DriveCmd
        (DriveCp (CpOpts ("Documents" :| ["report.pdf"]) Nothing Nothing defaultOpts))

  it "parses drive cp PATH --root DIR" $
    parseCmd ["drive", "cp", "Documents/Work/report.pdf", "--root", "/tmp/dl"]
      `endsRight` DriveCmd
        (DriveCp (CpOpts ("Documents" :| ["Work", "report.pdf"]) (Just "/tmp/dl") Nothing defaultOpts))

  it "parses drive cp PATH --output FILE" $
    parseCmd ["drive", "cp", "Documents/report.pdf", "--output", "/tmp/report.pdf"]
      `endsRight` DriveCmd
        (DriveCp (CpOpts ("Documents" :| ["report.pdf"]) Nothing (Just "/tmp/report.pdf") defaultOpts))

  it "parses drive cp single-segment PATH" $
    parseCmd ["drive", "cp", "report.pdf"]
      `endsRight` DriveCmd
        (DriveCp (CpOpts ("report.pdf" :| []) Nothing Nothing defaultOpts))

  it "parses drive cp PATH --root and --output together (conflict caught at runtime)" $
    parseCmd ["drive", "cp", "Documents/report.pdf", "--root", "/tmp/dl", "--output", "/tmp/out.pdf"]
      `endsRight` DriveCmd
        (DriveCp (CpOpts ("Documents" :| ["report.pdf"]) (Just "/tmp/dl") (Just "/tmp/out.pdf") defaultOpts))
