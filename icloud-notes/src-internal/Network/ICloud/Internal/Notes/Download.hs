{-# LANGUAGE OverloadedStrings #-}

module Network.ICloud.Internal.Notes.Download
  ( NotesError (..)
  , fetchFolders
  , fetchRecent
  , fetchNote
  , fetchNotesInFolder
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe, mapMaybe)
import Network.HTTP.Client
  ( Request
  , RequestBody (..)
  , Response (..)
  , requestBody
  , requestHeaders
  )
import Network.HTTP.Types (hContentType, statusCode)
import Network.ICloud.Http (Api, rawRequest)
import Network.ICloud.Internal.Notes.CloudKit
  ( CKLookupResponse (..)
  , CKQueryResponse (..)
  )
import Network.ICloud.Internal.Notes.Endpoints
  ( NotesEndpoints
  , foldersBody
  , lookupBody
  , lookupReq
  , notesInFolderBody
  , queryReq
  , recentsBody
  )
import Network.ICloud.Internal.Notes.Note
  ( FolderId
  , Note
  , NoteFolder
  , NoteId (..)
  , NoteSummary (..)
  )
import Network.ICloud.Internal.Notes.NoteData
  ( noteRecordToNote
  , parseFoldersFromQuery
  , parseSummariesFromQuery
  )


data NotesError
  = NotesHttpError Int
  | NotesParseError String
  deriving (Show)


instance Exception NotesError


fetchFolders :: Api -> NotesEndpoints -> IO [NoteFolder]
fetchFolders api ep = go Nothing []
 where
  go marker acc = do
    qr <- fetchAs "fetchFolders" api (jsonReq (foldersBody 200 marker) (queryReq ep))
    let acc' = acc <> parseFoldersFromQuery qr
    case qrContinuationMarker qr of
      Nothing -> pure acc'
      Just m -> go (Just m) acc'


fetchRecent :: Api -> NotesEndpoints -> IO [NoteSummary]
fetchRecent api ep = go Nothing []
 where
  go marker acc = do
    qr <- fetchAs "fetchRecent" api (jsonReq (recentsBody 200 marker) (queryReq ep))
    let acc' = acc <> parseSummariesFromQuery qr
    case qrContinuationMarker qr of
      Nothing -> pure acc'
      Just m -> go (Just m) acc'


fetchNote :: Api -> NotesEndpoints -> NoteId -> IO (Maybe Note)
fetchNote api ep nid = do
  lr <- fetchAs "fetchNote" api (jsonReq (lookupBody [unNoteId nid]) (lookupReq ep))
  pure $ listToMaybe $ mapMaybe noteRecordToNote (lrRecords lr)


fetchNotesInFolder :: Api -> NotesEndpoints -> FolderId -> IO [NoteSummary]
fetchNotesInFolder api ep fid = go Nothing []
 where
  go marker acc = do
    qr <- fetchAs "fetchNotesInFolder" api (jsonReq (notesInFolderBody fid 200 marker) (queryReq ep))
    let acc' = acc <> parseSummariesFromQuery qr
    case qrContinuationMarker qr of
      Nothing -> pure acc'
      Just m -> go (Just m) acc'


fetchAs :: (FromJSON a) => String -> Api -> Request -> IO a
fetchAs ctx api r = rawRequest' api r >>= decodeAs ctx


rawRequest' :: Api -> Request -> IO (Response LBS.ByteString)
rawRequest' api r = do
  resp <- rawRequest api r
  checkStatus resp
  pure resp


jsonReq :: LBS.ByteString -> Request -> Request
jsonReq body r =
  r
    { requestBody = RequestBodyLBS body
    , requestHeaders = (hContentType, "application/json") : requestHeaders r
    }


checkStatus :: Response a -> IO ()
checkStatus resp =
  let code = statusCode (responseStatus resp)
   in when (code >= 400) $ throwIO (NotesHttpError code)


decodeAs :: (FromJSON a) => String -> Response LBS.ByteString -> IO a
decodeAs ctx resp =
  case eitherDecode (responseBody resp) of
    Left err -> throwIO (NotesParseError (ctx <> ": " <> err))
    Right v -> pure v
