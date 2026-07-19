module Main where

import Control.Exception (catch, displayException)
import qualified Data.Text as Text
import Network.HTTP.Client.TLS (newTlsManager)
import Network.ICloud.Http
  ( Api
  , AuthError
  , AuthState (..)
  , fileLogger
  , login
  , mkApiWith
  , verboseLogger
  , withLogger
  )
import Network.ICloud.Http.Endpoints (Realm (..), realmEndpoints)
import Network.ICloud.Notes
import Network.ICloud.Session (loadSession)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode (..), stdout, withFile)


data Command
  = ListFolders CommonOpts
  | ListNotes ListNotesOpts


data ListNotesOpts = ListNotesOpts
  { lnFolder :: Maybe Text.Text
  , lnCommon :: CommonOpts
  }


data CommonOpts = CommonOpts
  { optChina :: Bool
  , optLog :: Bool
  , optLogFile :: Maybe FilePath
  , optLogBodies :: Bool
  }


commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "list-note-folders"
        ( info
            (ListFolders <$> commonOptsParser)
            (progDesc "List all iCloud Notes folders")
        )
        <> command
          "list-notes"
          ( info
              (ListNotes <$> listNotesOptsParser)
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
                <> metavar "ID"
                <> help "Folder record name (e.g. Folder/ABCD1234)"
            )
      )
    <*> commonOptsParser


commonOptsParser :: Parser CommonOpts
commonOptsParser =
  CommonOpts
    <$> switch (long "china" <> help "Use mainland China endpoints")
    <*> switch (long "log" <> help "Append HTTP exchanges to the default log file")
    <*> optional
      (strOption (long "log-file" <> metavar "FILE" <> help "Append HTTP exchanges to FILE"))
    <*> switch (long "log-bodies" <> help "Include request bodies in the HTTP exchange log")


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
      Just fid -> notesInFolder api ep (FolderId fid)
    mapM_ printNote notes


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
withNotesApi opts runAction = do
  session <- loadSession
  mgr <- newTlsManager
  let realm = if optChina opts then China else Usual
  api0 <- mkApiWith session (realmEndpoints realm) mgr
  mbLogPath <- resolveLogTarget opts
  let mkLogger = if optLogBodies opts then verboseLogger else fileLogger
      run api = do
        result <- login api
        case result of
          Authenticated sess ad -> do
            ep <- mkNotesEndpoints ad sess
            runAction api ep
          _ -> do
            putStrLn "Not authenticated — run 'icloud-auth login' first."
            exitFailure
      go = case mbLogPath of
        Just fp -> withFile fp AppendMode $ \h -> run (withLogger (mkLogger h) api0)
        Nothing
          | optLogBodies opts -> run (withLogger (mkLogger stdout) api0)
          | otherwise -> run api0
  go
    `catch` (\e -> onError (e :: AuthError))
    `catch` (\e -> onError (e :: NotesError))
 where
  onError e = putStrLn ("Error: " <> displayException e) >> exitFailure


resolveLogTarget :: CommonOpts -> IO (Maybe FilePath)
resolveLogTarget CommonOpts{optLogFile = Just fp} = pure (Just fp)
resolveLogTarget CommonOpts{optLog = True} = Just <$> defaultLogFile
resolveLogTarget _ = pure Nothing


defaultLogFile :: IO FilePath
defaultLogFile = do
  dir <- getUserCacheDir appDir
  createDirectoryIfMissing True dir
  pure (dir </> "requests.log")


appDir :: FilePath
appDir = "hs-icloud-notes"
