{-# LANGUAGE LambdaCase #-}

module Hstratus.Cli.Auth
  ( AuthCommand (..)
  , authParser
  , runAuth
  )
where

import Control.Exception (bracket_, catch, displayException)
import Data.String (fromString)
import Network.HStratus.Http
  ( AuthError
  , login
  , mkApiWith
  , withLogger
  )
import Network.HStratus.Http.Cli
  ( CommonOpts (..)
  , commonOptsParser
  , mkLoggerFor
  , resolveLogTarget
  )
import Network.HStratus.Http.Endpoints (Realm (..), realmEndpoints)
import Network.HStratus.Session (Credentials (..), loadSession, saveCredentials)
import Network.HTTP.Client.TLS (newTlsManager)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (IOMode (..), hFlush, hSetEcho, stdin, stdout, withFile)


data AuthCommand
  = AuthInit
  | AuthLogin CommonOpts
  deriving (Eq, Show)


authParser :: Parser AuthCommand
authParser =
  subparser
    ( command "init" (info (pure AuthInit) (progDesc "Save Apple ID credentials to the config directory"))
        <> command "login" (info (AuthLogin <$> commonOptsParser <**> helper) (progDesc "Authenticate with iCloud"))
    )


runAuth :: AuthCommand -> IO ()
runAuth = \case
  AuthInit -> runInit
  AuthLogin opts -> runLogin opts


runInit :: IO ()
runInit = do
  appleId <- prompt "Apple ID: "
  password <- promptSecret "Password: "
  saveCredentials (Credentials (fromString appleId) (fromString password))
  putStrLn "Credentials saved."


runLogin :: CommonOpts -> IO ()
runLogin opts = do
  session <- loadSession
  mgr <- newTlsManager
  let realm = if optChina opts then China else Usual
  api0 <- mkApiWith session (realmEndpoints realm) mgr
  mbLogPath <- resolveLogTarget opts
  let mkLogger' = mkLoggerFor opts
      go = case mbLogPath of
        Nothing -> login api0 >> putStrLn "Authenticated."
        Just fp -> withFile fp AppendMode $ \h ->
          login (withLogger (mkLogger' h) api0) >> putStrLn "Authenticated."
  go `catch` \e -> do
    putStrLn $ "Login failed: " <> displayException (e :: AuthError)
    exitFailure


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
