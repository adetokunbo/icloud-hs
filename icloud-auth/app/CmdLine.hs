{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception (bracket_, catch, displayException)
import Data.String (fromString)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.ICloud.Http (AuthError, fileLogger, login, mkApiWith, redactingLogger, withLogger)
import Network.ICloud.Http.Endpoints (Realm (..), realmEndpoints)
import Network.ICloud.Session (Credentials (..), loadSession, saveCredentials)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode (..), hFlush, hSetEcho, stdin, stdout, withFile)


data Command
  = Init
  | Login LoginOpts


data LoginOpts = LoginOpts
  { loginChina :: Bool
  , loginLog :: Bool
  , loginLogFile :: Maybe FilePath
  , loginRedact :: Bool
  }


commandParser :: Parser Command
commandParser =
  (Init <$ subparser (command "init" (info (pure ()) (progDesc "Save Apple ID credentials to the config directory"))))
    <|> (Login <$> loginOptsParser)


loginOptsParser :: Parser LoginOpts
loginOptsParser =
  LoginOpts
    <$> switch (long "china" <> help "Use mainland China endpoints")
    <*> switch (long "log" <> help "Append HTTP exchanges to the default log file")
    <*> optional (strOption (long "log-file" <> metavar "FILE" <> help "Append HTTP exchanges to FILE"))
    <*> switch (long "redact" <> help "Redact sensitive headers (tokens, cookies) in the log")


cliParser :: ParserInfo Command
cliParser =
  info
    (commandParser <**> helper)
    (fullDesc <> progDesc "icloud-auth: iCloud authentication tool")


main :: IO ()
main =
  execParser cliParser >>= \case
    Init -> runInit
    Login opts -> runLogin opts


runInit :: IO ()
runInit = do
  appleId <- prompt "Apple ID: "
  password <- promptSecret "Password: "
  saveCredentials (Credentials (fromString appleId) (fromString password))
  putStrLn "Credentials saved."


runLogin :: LoginOpts -> IO ()
runLogin opts = do
  session <- loadSession
  mgr <- newTlsManager
  let realm = if loginChina opts then China else Usual
  api0 <- mkApiWith session (realmEndpoints realm) mgr
  mbLogPath <- resolveLogTarget opts
  let mkLogger = if loginRedact opts then redactingLogger else fileLogger
      go = case mbLogPath of
        Nothing -> login api0 >> putStrLn "Authenticated."
        Just fp -> withFile fp AppendMode $ \h ->
          login (withLogger (mkLogger h) api0) >> putStrLn "Authenticated."
  go `catch` \e -> do
    putStrLn $ "Login failed: " <> displayException (e :: AuthError)
    exitFailure


resolveLogTarget :: LoginOpts -> IO (Maybe FilePath)
resolveLogTarget LoginOpts{loginLogFile = Just fp} = pure (Just fp)
resolveLogTarget LoginOpts{loginLog = True} = Just <$> defaultLogFile
resolveLogTarget _ = pure Nothing


defaultLogFile :: IO FilePath
defaultLogFile = do
  dir <- getUserCacheDir appDir
  createDirectoryIfMissing True dir
  pure (dir </> "requests.log")


appDir :: FilePath
appDir = "hs-icloud-auth"


prompt :: String -> IO String
prompt label = putStr label >> hFlush stdout >> getLine


promptSecret :: String -> IO String
promptSecret label = do
  putStr label
  hFlush stdout
  bracket_ (hSetEcho stdin False) (hSetEcho stdin True) $ do
    secret <- getLine
    putStrLn ""
    pure secret
