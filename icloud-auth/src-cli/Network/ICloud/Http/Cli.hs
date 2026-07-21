{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ICloud.Http.Cli
  ( -- * Common CLI options
    CommonOpts (..)
  , commonOptsParser

    -- * Log target resolution
  , resolveLogTarget
  , defaultLogFile

    -- * Logger selection
  , mkLoggerFor

    -- * Authenticated API runner
  , runWithApi
  )
where

import Control.Exception (catch, displayException)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.ICloud.Http
  ( Api
  , ApiLogger
  , AuthError
  , AuthState (..)
  , fileLogger
  , login
  , mkApiWith
  , redactingLogger
  , verboseLogger
  , withLogger
  )
import Network.ICloud.Http.Endpoints (Realm (..), realmEndpoints)
import Network.ICloud.Session (AccountData, Session, loadSession)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..), stdout, withFile)


-- | Options shared by all icloud CLI commands.
data CommonOpts = CommonOpts
  { optChina :: Bool
  -- ^ Use mainland China endpoints instead of the worldwide endpoints.
  , optLog :: Bool
  -- ^ Append HTTP exchanges to the default log file.
  , optLogFile :: Maybe FilePath
  -- ^ Append HTTP exchanges to this file instead of the default.
  , optLogBodies :: Bool
  -- ^ Include request bodies in the HTTP exchange log.
  , optRedact :: Bool
  -- ^ Redact sensitive headers (tokens, cookies) in the log.
  }


-- | Parser for 'CommonOpts'.
commonOptsParser :: Parser CommonOpts
commonOptsParser =
  CommonOpts
    <$> switch (long "china" <> help "Use mainland China endpoints")
    <*> switch (long "log" <> help "Append HTTP exchanges to the default log file")
    <*> optional
      (strOption (long "log-file" <> metavar "FILE" <> help "Append HTTP exchanges to FILE"))
    <*> switch (long "log-bodies" <> help "Include request bodies in the HTTP exchange log")
    <*> switch (long "redact" <> help "Redact sensitive headers (tokens, cookies) in the log")


-- | Resolve the log file path from 'CommonOpts', or 'Nothing' if logging is disabled.
resolveLogTarget :: CommonOpts -> IO (Maybe FilePath)
resolveLogTarget CommonOpts{optLogFile = Just fp} = pure (Just fp)
resolveLogTarget CommonOpts{optLog = True} = Just <$> defaultLogFile
resolveLogTarget _ = pure Nothing


-- | Default log file path: @~\/.cache\/hs-icloud\/requests.log@.
defaultLogFile :: IO FilePath
defaultLogFile = do
  dir <- getUserCacheDir "hs-icloud"
  createDirectoryIfMissing True dir
  pure (dir </> "requests.log")


-- | Select the appropriate logger constructor from 'CommonOpts'.
mkLoggerFor :: CommonOpts -> Handle -> ApiLogger
mkLoggerFor CommonOpts{optRedact = True} = redactingLogger
mkLoggerFor CommonOpts{optLogBodies = True} = verboseLogger
mkLoggerFor _ = fileLogger


{- | Authenticate and run an action with the resulting 'Api'.

Handles session loading, TLS manager creation, logger wiring, and catches
'AuthError'. Additional error types should be caught by the caller.
-}
runWithApi
  :: CommonOpts
  -> (AccountData -> Session -> Api -> IO ())
  -> IO ()
runWithApi opts runAction = do
  session <- loadSession
  mgr <- newTlsManager
  let realm = if optChina opts then China else Usual
  api0 <- mkApiWith session (realmEndpoints realm) mgr
  mbLogPath <- resolveLogTarget opts
  let mkLogger' = mkLoggerFor opts
      run api = do
        result <- login api
        case result of
          Authenticated sess ad -> runAction ad sess api
          _ -> putStrLn "Not authenticated — run 'icloud-auth login' first." >> exitFailure
      go = case mbLogPath of
        Just fp -> withFile fp AppendMode $ \h -> run (withLogger (mkLogger' h) api0)
        Nothing
          | optLogBodies opts && not (optRedact opts) -> run (withLogger (mkLogger' stdout) api0)
          | otherwise -> run api0
  go `catch` \e -> putStrLn ("Error: " <> displayException (e :: AuthError)) >> exitFailure
