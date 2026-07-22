module Hstratus.Cli
  ( TopCommand (..)
  , cliParser
  , run
  )
where

import Hstratus.Cli.Auth (AuthCommand, authParser, runAuth)
import Options.Applicative


data TopCommand
  = AuthCmd AuthCommand
  deriving (Eq, Show)


cliParser :: ParserInfo TopCommand
cliParser =
  info
    (topParser <**> helper)
    (fullDesc <> progDesc "hstratus: iCloud service tools")


topParser :: Parser TopCommand
topParser =
  subparser
    (command "auth" (info (AuthCmd <$> authParser <**> helper) (progDesc "iCloud authentication commands")))


run :: IO ()
run = execParser cliParser >>= dispatch


dispatch :: TopCommand -> IO ()
dispatch (AuthCmd cmd) = runAuth cmd
