{-# LANGUAGE OverloadedStrings #-}

module Hstratus.Cli.NotesSpec (spec) where

import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Notes (ListNotesOpts (..), NotesCommand (..))
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
spec = describe "notes parser" $ do
  it "parses notes list-note-folders" $
    parseCmd ["notes", "list-note-folders"]
      `endsRight` NotesCmd (NotesListFolders defaultOpts)

  it "parses notes list-notes --folder NAME" $
    parseCmd ["notes", "list-notes", "--folder", "TukTuk"]
      `endsRight` NotesCmd (NotesListNotes (ListNotesOpts (Just "TukTuk") defaultOpts))
