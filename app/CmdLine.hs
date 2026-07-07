{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception (catch, displayException)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.ICloud.Http (AuthError, fileLogger, login, mkApiWith, withLogger)
import Network.ICloud.Http.Endpoints (Realm (..), realmEndpoints)
import Network.ICloud.Session (loadSession)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (IOMode (..), withFile)


data Opts = Opts
  { optChina :: Bool
  , optLog :: Maybe FilePath
  }


optsParser :: Parser Opts
optsParser =
  Opts
    <$> switch (long "china" <> help "Use mainland China endpoints")
    <*> optional (strOption (long "log" <> metavar "FILE" <> help "Append HTTP exchanges to FILE"))


cliParser :: ParserInfo Opts
cliParser =
  info
    (optsParser <**> helper)
    (fullDesc <> progDesc "Sign in to iCloud and cache the session token")


main :: IO ()
main = do
  opts <- execParser cliParser
  session <- loadSession
  mgr <- newTlsManager
  let realm = if optChina opts then China else Usual
  api0 <- mkApiWith session (realmEndpoints realm) mgr
  let runLogin api = login api >> putStrLn "Authenticated."
  let go = case optLog opts of
        Nothing -> runLogin api0
        Just fp -> withFile fp AppendMode $ \h ->
          runLogin (withLogger (fileLogger h) api0)
  go `catch` \e -> do
    putStrLn $ "Login failed: " <> displayException (e :: AuthError)
    exitFailure
