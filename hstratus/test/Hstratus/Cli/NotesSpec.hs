{-# LANGUAGE OverloadedStrings #-}

module Hstratus.Cli.NotesSpec (spec) where

import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Notes (ListNotesOpts (..), NotesCommand (..))
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
spec = describe "notes parser" $ do
  it "parses notes list-note-folders" $ do
    case parseCmd ["notes", "list-note-folders"] of
      Right (NotesCmd (NotesListFolders _)) -> pure ()
      other -> expectationFailure $ "unexpected result: " <> show other

  it "parses notes list-notes --folder NAME" $ do
    case parseCmd ["notes", "list-notes", "--folder", "TukTuk"] of
      Right (NotesCmd (NotesListNotes opts)) ->
        lnFolder opts `shouldBe` Just "TukTuk"
      other -> expectationFailure $ "unexpected result: " <> show other
