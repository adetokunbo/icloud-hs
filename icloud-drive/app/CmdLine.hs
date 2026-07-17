module Main where

import Control.Exception (catch, displayException)
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client.TLS (newTlsManager)
import Network.ICloud.Drive (listAppLibrariesRaw, mkDriveEndpoints)
import Network.ICloud.Http (AuthError, AuthState (..), fileLogger, login, mkApiWith, withLogger)
import Network.ICloud.Http.Endpoints (Realm (..), realmEndpoints)
import Network.ICloud.Session (loadSession)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)


data Command
  = ListAppLibraries ListOpts


data ListOpts = ListOpts
  { listChina :: Bool
  , listLog :: Bool
  , listLogFile :: Maybe FilePath
  }


commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "list-app-libraries"
        ( info
            (ListAppLibraries <$> listOptsParser)
            (progDesc "Print raw JSON from retrieveAppLibraries")
        )
    )


listOptsParser :: Parser ListOpts
listOptsParser =
  ListOpts
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


runListAppLibraries :: ListOpts -> IO ()
runListAppLibraries opts = do
  session <- loadSession
  mgr <- newTlsManager
  let realm = if listChina opts then China else Usual
  api0 <- mkApiWith session (realmEndpoints realm) mgr
  mbLogPath <- resolveLogTarget opts
  let run api = do
        result <- login api
        case result of
          Authenticated sess ad -> do
            ep <- mkDriveEndpoints ad sess
            raw <- listAppLibrariesRaw api ep
            LBS.putStr raw
            putStrLn ""
          _ -> do
            putStrLn "Not authenticated — run 'icloud-auth login' first."
            exitFailure
  let go = case mbLogPath of
        Nothing -> run api0
        Just fp -> withFile fp AppendMode $ \h -> run (withLogger (fileLogger h) api0)
  go `catch` \e -> do
    putStrLn $ "Error: " <> displayException (e :: AuthError)
    exitFailure


resolveLogTarget :: ListOpts -> IO (Maybe FilePath)
resolveLogTarget ListOpts{listLogFile = Just fp} = pure (Just fp)
resolveLogTarget ListOpts{listLog = True} = Just <$> defaultLogFile
resolveLogTarget _ = pure Nothing


defaultLogFile :: IO FilePath
defaultLogFile = do
  dir <- getUserCacheDir appDir
  createDirectoryIfMissing True dir
  pure (dir </> "requests.log")


appDir :: FilePath
appDir = "hs-icloud-drive"
