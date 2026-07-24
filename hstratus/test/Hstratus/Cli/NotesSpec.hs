{-# LANGUAGE OverloadedStrings #-}

module Hstratus.Cli.NotesSpec (spec) where

import Hstratus.Cli (TopCommand (..), cliParser)
import Hstratus.Cli.Notes (ListNotesOpts (..), NotesCommand (..), findFolderByName)
import Network.HStratus.Http.Cli (CommonOpts (..))
import Network.HStratus.Notes.Note (FolderId (..), NoteFolder (..))
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
spec = do
  describe "notes parser" $ do
    it "parses notes list-note-folders" $
      parseCmd ["notes", "list-note-folders"]
        `endsRight` NotesCmd (NotesListFolders defaultOpts)

    it "parses notes list-notes --folder NAME" $
      parseCmd ["notes", "list-notes", "--folder", "TukTuk"]
        `endsRight` NotesCmd (NotesListNotes (ListNotesOpts (Just "TukTuk") defaultOpts))

  describe "findFolderByName" $ do
    it "returns Just FolderId on an exact-case match" $
      findFolderByName "Work" testFolders `shouldBe` Just (FolderId "Folder/WORK")

    it "returns Just FolderId on a case-insensitive match" $
      findFolderByName "work" testFolders `shouldBe` Just (FolderId "Folder/WORK")

    it "returns Nothing when no folder matches" $
      findFolderByName "Missing" testFolders `shouldBe` Nothing

    it "returns Nothing for a folder whose name is absent" $
      findFolderByName "Work" [NoteFolder (FolderId "Folder/UNNAMED") Nothing] `shouldBe` Nothing


testFolders :: [NoteFolder]
testFolders =
  [ NoteFolder (FolderId "Folder/WORK") (Just "Work")
  , NoteFolder (FolderId "Folder/PERSONAL") (Just "Personal")
  ]
