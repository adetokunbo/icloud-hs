module Hstratus.Cli.Drive
  ( DriveCommand (..)
  , ListFolderOpts (..)
  , CpOpts (..)
  , driveParser
  , runDrive
  )
where

import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import qualified Data.Text as Text
import Network.HStratus.Drive
  ( DriveApi
  , DriveNode (..)
  , DriveNodeId
  , FileData (..)
  , FolderData (..)
  , downloadFile
  , driveRoot
  , fileName
  , listFolder
  , mkDriveApi
  )
import Network.HStratus.Http.Cli (CommonOpts (..), commonOptsParser, runWithApi)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Exit (die)
import System.FilePath (joinPath, takeDirectory, (</>))


data DriveCommand
  = DriveListRoot CommonOpts
  | DriveListFolder ListFolderOpts
  | DriveCp CpOpts
  deriving (Eq, Show)


data ListFolderOpts = ListFolderOpts
  { lfPath :: [Text.Text]
  , lfCommon :: CommonOpts
  }
  deriving (Eq, Show)


data CpOpts = CpOpts
  { cpSrcPath :: [Text.Text]
  , cpRoot :: Maybe FilePath
  , cpOutput :: Maybe FilePath
  , cpCommon :: CommonOpts
  }
  deriving (Eq, Show)


driveParser :: Parser DriveCommand
driveParser =
  subparser
    ( command
        "list-root"
        ( info
            (DriveListRoot <$> commonOptsParser <**> helper)
            (progDesc "List immediate children of the top-level iCloud Drive folder")
        )
        <> command
          "list-folder"
          ( info
              (DriveListFolder <$> listFolderOptsParser <**> helper)
              (progDesc "List contents of a folder at a slash-separated path from root")
          )
        <> command
          "cp"
          ( info
              (DriveCp <$> cpOptsParser <**> helper)
              (progDesc "Download a file from Drive to the local filesystem")
          )
    )


cpOptsParser :: Parser CpOpts
cpOptsParser =
  CpOpts
    <$> fmap
      (filter (not . Text.null) . Text.splitOn (Text.pack "/") . Text.pack)
      (argument str (metavar "PATH" <> help "Slash-separated path to the file in Drive"))
    <*> optional (strOption (long "root" <> metavar "DIR" <> help "Copy under DIR, mirroring the Drive path"))
    <*> optional (strOption (long "output" <> metavar "FILE" <> help "Copy to the exact local path FILE"))
    <*> commonOptsParser


listFolderOptsParser :: Parser ListFolderOpts
listFolderOptsParser =
  ListFolderOpts
    <$> fmap
      (filter (not . Text.null) . Text.splitOn (Text.pack "/") . Text.pack)
      (argument str (metavar "PATH" <> help "Slash-separated path from root (e.g. Documents/Work)"))
    <*> commonOptsParser


runDrive :: DriveCommand -> IO ()
runDrive (DriveListRoot opts) = runListRoot opts
runDrive (DriveListFolder opts) = runListFolder opts
runDrive (DriveCp opts) = runCp opts


runCp :: CpOpts -> IO ()
runCp opts = case (cpRoot opts, cpOutput opts) of
  (Just _, Just _) ->
    die "Error: --root and --output cannot both be specified"
  _ ->
    withDriveApi (cpCommon opts) $ \da -> do
      root <- driveRoot da
      fd <- navigateToFile da (fnId root) (cpSrcPath opts)
      dest <- resolveLocalDest opts (cpSrcPath opts)
      createDirectoryIfMissing True (takeDirectory dest)
      bytes <- downloadFile da fd
      LBS.writeFile dest bytes
      putStrLn $ "Downloaded to " <> dest


navigateToFile :: DriveApi -> DriveNodeId -> [Text.Text] -> IO FileData
navigateToFile _ _ [] = die "cp: PATH must not be empty"
navigateToFile da nid [name] = do
  children <- listFolder da nid
  case find (matchesName name) children of
    Just (DriveFile fd) -> pure fd
    Just (DriveFolder _) -> die $ "Not a file: " <> Text.unpack name
    Nothing -> die $ "File not found: " <> Text.unpack name
 where
  matchesName n (DriveFile fd) = fileName fd == n
  matchesName n (DriveFolder fd) = fnName fd == n
navigateToFile da nid (seg : segs) = do
  children <- listFolder da nid
  case find (matchFolderName seg) children of
    Nothing -> die $ "Folder not found: " <> Text.unpack seg
    Just (DriveFile _) -> die $ "Not a folder: " <> Text.unpack seg
    Just (DriveFolder fd) -> navigateToFile da (fnId fd) segs


resolveLocalDest :: CpOpts -> [Text.Text] -> IO FilePath
resolveLocalDest (CpOpts{cpOutput = Just out}) _ = pure out
resolveLocalDest (CpOpts{cpRoot = Just root}) segs =
  pure $ root </> joinPath (map Text.unpack segs)
resolveLocalDest _ segs = do
  home <- getHomeDirectory
  pure $ home </> "icloud-drive" </> joinPath (map Text.unpack segs)


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
    Nothing -> die $ "Folder not found: " <> Text.unpack seg
    Just (DriveFile _) -> die $ "Not a folder: " <> Text.unpack seg
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
