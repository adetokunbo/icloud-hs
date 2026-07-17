module Main where

import Control.Exception (catch, displayException)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Network.HTTP.Client.TLS (newTlsManager)
import Network.ICloud.Drive
  ( CloudScope
  , DriveEndpoints
  , DriveNode (..)
  , FileData (..)
  , FolderData (..)
  , driveRoot
  , fileName
  , fnId
  , fnName
  , listAppLibrariesRaw
  , listFolder
  , mkDriveEndpoints
  )
import Network.ICloud.Http (Api, AuthError, AuthState (..), fileLogger, login, mkApiWith, withLogger)
import Network.ICloud.Http.Endpoints (Realm (..), realmEndpoints)
import Network.ICloud.Session (loadSession)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)


data Command
  = ListAppLibraries CommonOpts
  | ListRoot CommonOpts


data CommonOpts = CommonOpts
  { optChina :: Bool
  , optLog :: Bool
  , optLogFile :: Maybe FilePath
  }


commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "list-app-libraries"
        ( info
            (ListAppLibraries <$> commonOptsParser)
            (progDesc "Print raw JSON from retrieveAppLibraries")
        )
        <> command
          "list-root"
          ( info
              (ListRoot <$> commonOptsParser)
              (progDesc "List immediate children of the top-level iCloud Drive folder")
          )
    )


commonOptsParser :: Parser CommonOpts
commonOptsParser =
  CommonOpts
    <$> switch (long "china" <> help "Use mainland China endpoints")
    <*> switch (long "log" <> help "Append HTTP exchanges to the default log file")
    <*> optional
      (strOption (long "log-file" <> metavar "FILE" <> help "Append HTTP exchanges to FILE"))


cliParser :: ParserInfo Command
cliParser =
  info
    (commandParser <**> helper)
    (fullDesc <> progDesc "icloud-drive: iCloud Drive access tool")


main :: IO ()
main = do
  cmd <- execParser cliParser
  case cmd of
    ListAppLibraries opts -> runListAppLibraries opts
    ListRoot opts -> runListRoot opts


runListAppLibraries :: CommonOpts -> IO ()
runListAppLibraries opts =
  withDriveApi opts $ \api ep -> do
    raw <- listAppLibrariesRaw api ep
    LBS.putStr raw
    putStrLn ""


runListRoot :: CommonOpts -> IO ()
runListRoot opts =
  withDriveApi opts $ \api ep -> do
    root <- driveRoot api ep
    nodes <- listFolder api ep (fnId root)
    mapM_ printNode nodes


printNode :: DriveNode -> IO ()
printNode (DriveFolder fd) =
  putStrLn $ "FOLDER  " <> Text.unpack (fnName fd)
printNode (DriveFile fd) =
  putStrLn $ "FILE    " <> Text.unpack (fileName fd) <> sizeStr
 where
  sizeStr = case fdSize fd of
    Nothing -> ""
    Just n -> "  (" <> show n <> " bytes)"


withDriveApi :: CommonOpts -> (Api -> DriveEndpoints CloudScope -> IO ()) -> IO ()
withDriveApi opts action = do
  session <- loadSession
  mgr <- newTlsManager
  let realm = if optChina opts then China else Usual
  api0 <- mkApiWith session (realmEndpoints realm) mgr
  mbLogPath <- resolveLogTarget opts
  let run api = do
        result <- login api
        case result of
          Authenticated sess ad -> do
            ep <- mkDriveEndpoints ad sess
            action api ep
          _ -> do
            putStrLn "Not authenticated — run 'icloud-auth login' first."
            exitFailure
  let go = case mbLogPath of
        Nothing -> run api0
        Just fp -> withFile fp AppendMode $ \h -> run (withLogger (fileLogger h) api0)
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
