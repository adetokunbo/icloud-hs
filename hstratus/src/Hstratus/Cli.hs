module Hstratus.Cli
  ( TopCommand (..)
  , cliParser
  , run
  )
where

import Hstratus.Cli.Auth (AuthCommand, authParser, runAuth)
import Hstratus.Cli.Drive (DriveCommand, driveParser, runDrive)
import Hstratus.Cli.Notes (NotesCommand, notesParser, runNotes)
import Options.Applicative


data TopCommand
  = AuthCmd AuthCommand
  | DriveCmd DriveCommand
  | NotesCmd NotesCommand
  deriving (Eq, Show)


cliParser :: ParserInfo TopCommand
cliParser =
  info
    (topParser <**> helper)
    (fullDesc <> progDesc "hstratus: iCloud service tools")


topParser :: Parser TopCommand
topParser =
  subparser
    ( command "auth" (info (AuthCmd <$> authParser <**> helper) (progDesc "iCloud authentication commands"))
        <> command "drive" (info (DriveCmd <$> driveParser <**> helper) (progDesc "iCloud Drive commands"))
        <> command "notes" (info (NotesCmd <$> notesParser <**> helper) (progDesc "iCloud Notes commands"))
    )


run :: IO ()
run = execParser cliParser >>= dispatch


dispatch :: TopCommand -> IO ()
dispatch (AuthCmd cmd) = runAuth cmd
dispatch (DriveCmd cmd) = runDrive cmd
dispatch (NotesCmd cmd) = runNotes cmd
