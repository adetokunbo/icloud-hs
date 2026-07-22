module Main where

import Data.List (find)
import qualified Data.Text as Text
import Network.HStratus.Drive
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
import Network.HStratus.Http.Cli (CommonOpts (..), commonOptsParser, runWithApi)
import Options.Applicative


data Command
  = ListRoot CommonOpts
  | ListFolder ListFolderOpts


data ListFolderOpts = ListFolderOpts
  { lfPath :: [Text.Text]
  , lfCommon :: CommonOpts
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


cliParser :: ParserInfo Command
cliParser =
  info
    (commandParser <**> helper)
    (fullDesc <> progDesc "hstratus-drive: iCloud Drive access tool")


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
withDriveApi opts runAction =
  runWithApi opts $ \ad sess api -> do
    da <- mkDriveApi ad sess api
    runAction da
