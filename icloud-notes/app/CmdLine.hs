module Main where

import Control.Exception (Exception, catch, displayException)
import Data.List (find)
import qualified Data.Text as Text
import Network.HStratus.Http (Api, AuthError)
import Network.HStratus.Http.Cli (CommonOpts (..), commonOptsParser, runWithApi)
import Network.HStratus.Notes
import Options.Applicative
import System.Exit (exitFailure)


data Command
  = ListFolders CommonOpts
  | ListNotes ListNotesOpts


data ListNotesOpts = ListNotesOpts
  { lnFolder :: Maybe Text.Text
  , lnCommon :: CommonOpts
  }


commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "list-note-folders"
        ( info
            (ListFolders <$> commonOptsParser <**> helper)
            (progDesc "List all iCloud Notes folders")
        )
        <> command
          "list-notes"
          ( info
              (ListNotes <$> listNotesOptsParser <**> helper)
              (progDesc "List notes, optionally filtered by folder ID")
          )
    )


listNotesOptsParser :: Parser ListNotesOpts
listNotesOptsParser =
  ListNotesOpts
    <$> optional
      ( Text.pack
          <$> strOption
            ( long "folder"
                <> metavar "NAME"
                <> help "Folder name (e.g. TukTuk)"
            )
      )
    <*> commonOptsParser


cliParser :: ParserInfo Command
cliParser =
  info
    (commandParser <**> helper)
    (fullDesc <> progDesc "icloud-notes: iCloud Notes access tool")


main :: IO ()
main = do
  cmd <- execParser cliParser
  case cmd of
    ListFolders opts -> runListFolders opts
    ListNotes opts -> runListNotes opts


runListFolders :: CommonOpts -> IO ()
runListFolders opts =
  withNotesApi opts $ \api ep ->
    noteFolders api ep >>= mapM_ printFolder


runListNotes :: ListNotesOpts -> IO ()
runListNotes opts =
  withNotesApi (lnCommon opts) $ \api ep -> do
    notes <- case lnFolder opts of
      Nothing -> recentNotes api ep
      Just name -> do
        fid <- resolveFolderName api ep name
        notesInFolder api ep fid
    mapM_ printNote notes


resolveFolderName :: Api -> NotesEndpoints -> Text.Text -> IO FolderId
resolveFolderName api ep name = do
  folders <- noteFolders api ep
  case find (matchesName name) folders of
    Just nf -> pure (nfId nf)
    Nothing -> do
      putStrLn $ "No folder named '" <> Text.unpack name <> "'"
      exitFailure
 where
  matchesName n nf = maybe False (\fn -> Text.toCaseFold fn == Text.toCaseFold n) (nfName nf)


printFolder :: NoteFolder -> IO ()
printFolder nf =
  putStrLn $ Text.unpack (unFolderId (nfId nf)) <> nameStr
 where
  nameStr = maybe "" (("  " <>) . Text.unpack) (nfName nf)


printNote :: NoteSummary -> IO ()
printNote ns =
  putStrLn $ Text.unpack (unNoteId (nsId ns)) <> titleStr
 where
  titleStr = maybe "" (("  " <>) . Text.unpack) (nsTitle ns)


withNotesApi :: CommonOpts -> (Api -> NotesEndpoints -> IO ()) -> IO ()
withNotesApi opts runAction =
  runWithApi opts (\ad sess api -> mkNotesEndpoints ad sess >>= runAction api)
    `catch` (\e -> onError (e :: AuthError))
    `catch` (\e -> onError (e :: NotesError))
 where
  onError :: (Exception a) => a -> IO ()
  onError e = putStrLn ("Error: " <> displayException e) >> exitFailure
