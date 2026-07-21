module Main where

import Control.Exception (catch, displayException)
import Data.List (find)
import qualified Data.Text as Text
import Network.HTTP.Client.TLS (newTlsManager)
import Network.ICloud.Drive
  ( DriveApi
  , DriveNode (..)
  , DriveNodeId
  , FileData (..)
  , FolderData (..)
  , driveRoot
  , fileName
  , listFolder
  , mkDriveApi
  )
import Network.ICloud.Http (AuthError, AuthState (..), fileLogger, login, mkApiWith, redactingLogger, verboseLogger, withLogger)
import Network.ICloud.Http.Endpoints (Realm (..), realmEndpoints)
import Network.ICloud.Session (loadSession)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode (..), stdout, withFile)


data Command
  = ListRoot CommonOpts
  | ListFolder ListFolderOpts


data ListFolderOpts = ListFolderOpts
  { lfPath :: [Text.Text]
  , lfCommon :: CommonOpts
  }


data CommonOpts = CommonOpts
  { optChina :: Bool
  , optLog :: Bool
  , optLogFile :: Maybe FilePath
  , optLogBodies :: Bool
  , optRedact :: Bool
  }


commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "list-root"
        ( info
            (ListRoot <$> commonOptsParser <**> helper)
            (progDesc "List immediate children of the top-level iCloud Drive folder")
        )
        <> command
          "list-folder"
          ( info
              (ListFolder <$> listFolderOptsParser <**> helper)
              (progDesc "List contents of a folder at a slash-separated path from root")
          )
    )


listFolderOptsParser :: Parser ListFolderOpts
listFolderOptsParser =
  ListFolderOpts
    <$> fmap (filter (not . Text.null) . Text.splitOn (Text.pack "/") . Text.pack) (argument str (metavar "PATH" <> help "Slash-separated path from root (e.g. Documents/Work)"))
    <*> commonOptsParser


commonOptsParser :: Parser CommonOpts
commonOptsParser =
  CommonOpts
    <$> switch (long "china" <> help "Use mainland China endpoints")
    <*> switch (long "log" <> help "Append HTTP exchanges to the default log file")
    <*> optional
      (strOption (long "log-file" <> metavar "FILE" <> help "Append HTTP exchanges to FILE"))
    <*> switch (long "log-bodies" <> help "Include request bodies in the HTTP exchange log")
    <*> switch (long "redact" <> help "Redact sensitive headers (tokens, cookies) in the log")


cliParser :: ParserInfo Command
cliParser =
  info
    (commandParser <**> helper)
    (fullDesc <> progDesc "icloud-drive: iCloud Drive access tool")


main :: IO ()
main = do
  cmd <- execParser cliParser
  case cmd of
    ListRoot opts -> runListRoot opts
    ListFolder opts -> runListFolder opts


runListRoot :: CommonOpts -> IO ()
runListRoot opts =
  withDriveApi opts $ \da -> do
    root <- driveRoot da
    nodes <- listFolder da (fnId root)
    mapM_ printNode nodes


runListFolder :: ListFolderOpts -> IO ()
runListFolder opts =
  withDriveApi (lfCommon opts) $ \da -> do
    root <- driveRoot da
    nid <- navigatePath da (fnId root) (lfPath opts)
    nodes <- listFolder da nid
    mapM_ printNode nodes


navigatePath :: DriveApi -> DriveNodeId -> [Text.Text] -> IO DriveNodeId
navigatePath _ nid [] = pure nid
navigatePath da nid (seg : segs) = do
  children <- listFolder da nid
  case find (matchFolderName seg) children of
    Nothing -> fail $ "Folder not found: " <> Text.unpack seg
    Just (DriveFile _) -> fail $ "Not a folder: " <> Text.unpack seg
    Just (DriveFolder fd) -> navigatePath da (fnId fd) segs


matchFolderName :: Text.Text -> DriveNode -> Bool
matchFolderName name (DriveFolder fd) = fnName fd == name
matchFolderName _ (DriveFile _) = False


printNode :: DriveNode -> IO ()
printNode (DriveFolder fd) =
  putStrLn $ "FOLDER  " <> Text.unpack (fnName fd)
printNode (DriveFile fd) =
  putStrLn $ "FILE    " <> Text.unpack (fileName fd) <> sizeStr
 where
  sizeStr = case fdSize fd of
    Nothing -> ""
    Just n -> "  (" <> show n <> " bytes)"


withDriveApi :: CommonOpts -> (DriveApi -> IO ()) -> IO ()
withDriveApi opts runAction = do
  session <- loadSession
  mgr <- newTlsManager
  let realm = if optChina opts then China else Usual
  api0 <- mkApiWith session (realmEndpoints realm) mgr
  mbLogPath <- resolveLogTarget opts
  let mkLogger
        | optRedact opts = redactingLogger
        | optLogBodies opts = verboseLogger
        | otherwise = fileLogger
      run api = do
        result <- login api
        case result of
          Authenticated sess ad -> do
            da <- mkDriveApi ad sess api
            runAction da
          _ -> do
            putStrLn "Not authenticated — run 'icloud-auth login' first."
            exitFailure
  let go = case mbLogPath of
        Just fp -> withFile fp AppendMode $ \h -> run (withLogger (mkLogger h) api0)
        Nothing
          | optLogBodies opts && not (optRedact opts) -> run (withLogger (mkLogger stdout) api0)
          | otherwise -> run api0
  go `catch` \e -> do
    putStrLn $ "Error: " <> displayException (e :: AuthError)
    exitFailure


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
appDir = "hs-icloud-drive"
