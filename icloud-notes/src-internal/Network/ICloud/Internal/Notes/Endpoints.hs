{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ICloud.Internal.Notes.Endpoints
  ( NotesEndpoints (..)
  , mkNotesEndpoints
  , queryReq
  , lookupReq
  , changesReq
  , foldersBody
  , recentsBody
  , lookupBody
  , changesBody
  )
where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client
  ( Request (..)
  , parseRequest
  )
import Network.HTTP.Types (HeaderName, hAccept, hReferer, hUserAgent, methodPost)
import Network.ICloud.Session (AccountData (..), Session (..))


-- | Base request for the iCloud Notes CloudKit database API.
data NotesEndpoints = NotesEndpoints
  { neBaseReq :: !Request
  , neQueryString :: !BS8.ByteString
  }


{- | Construct 'NotesEndpoints' from the account data returned after login.

Fails if the @ckdatabasews@ service URL is absent from the account data.
-}
mkNotesEndpoints :: AccountData -> Session -> IO NotesEndpoints
mkNotesEndpoints ad sess = do
  svcReq <- lookupAndParse "ckdatabasews" (adWebservices ad)
  let baseReq =
        addNotesHeaders $
          svcReq
            { path =
                stripTrailingSlash (path svcReq)
                  <> "/database/1/com.apple.notes/production/private"
            }
      qs =
        "remapEnums=true&getCurrentSyncToken=true&clientId="
          <> BS8.pack (Text.unpack (sessionClientId sess))
  pure NotesEndpoints{neBaseReq = baseReq, neQueryString = qs}


-- | Build the @POST /records/query@ request.
queryReq :: NotesEndpoints -> Request
queryReq ep =
  (neBaseReq ep)
    { path = path (neBaseReq ep) <> "/records/query"
    , method = methodPost
    , queryString = neQueryString ep
    }


-- | Build the @POST /records/lookup@ request.
lookupReq :: NotesEndpoints -> Request
lookupReq ep =
  (neBaseReq ep)
    { path = path (neBaseReq ep) <> "/records/lookup"
    , method = methodPost
    , queryString = neQueryString ep
    }


-- | Build the @POST /changes/zone@ request.
changesReq :: NotesEndpoints -> Request
changesReq ep =
  (neBaseReq ep)
    { path = path (neBaseReq ep) <> "/changes/zone"
    , method = methodPost
    , queryString = neQueryString ep
    }


{- | Build the JSON body for a folders query.  Pass the previous response's
@continuationMarker@ to page through results.
-}
foldersBody :: Int -> Maybe Value -> LBS.ByteString
foldersBody limit marker = encode $ object $ base <> cont
 where
  base =
    [ "query"
        .= object
          [ "recordType" .= ("SearchIndexes" :: Text)
          , "filterBy" .= [indexFilter "parentless"]
          ]
    , "zoneID" .= notesZoneId
    , "resultsLimit" .= min 200 limit
    ]
  cont = maybe [] (\m -> ["continuationMarker" .= m]) marker


{- | Build the JSON body for a recent-notes query.  Pass the previous
response's @continuationMarker@ to page through results.
-}
recentsBody :: Int -> Maybe Value -> LBS.ByteString
recentsBody limit marker = encode $ object $ base <> cont
 where
  base =
    [ "query"
        .= object
          [ "recordType" .= ("Note" :: Text)
          , "filterBy" .= [indexFilter "recents"]
          , "sortBy"
              .= [ object
                     [ "fieldName" .= ("modTime" :: Text)
                     , "ascending" .= False
                     ]
                 ]
          ]
    , "zoneID" .= notesZoneId
    , "resultsLimit" .= min 200 limit
    ]
  cont = maybe [] (\m -> ["continuationMarker" .= m]) marker


-- | Build the JSON body for a record lookup by name.
lookupBody :: [Text] -> LBS.ByteString
lookupBody names =
  encode $
    object
      [ "records" .= map (\n -> object ["recordName" .= n]) names
      , "zoneID" .= notesZoneId
      ]


{- | Build the JSON body for a zone-changes request.  Pass the previous
@syncToken@ to fetch only changes since that token.
-}
changesBody :: Maybe Text -> LBS.ByteString
changesBody syncToken =
  encode $
    object
      ["zones" .= [object $ zoneBase <> syncPart]]
 where
  zoneBase =
    [ "zoneID" .= notesZoneId
    , "desiredRecordTypes" .= (["Note"] :: [Text])
    ]
  syncPart = maybe [] (\t -> ["syncToken" .= t]) syncToken


-- Helpers

notesZoneId :: Value
notesZoneId =
  object
    [ "zoneName" .= ("Notes" :: Text)
    , "zoneType" .= ("REGULAR_CUSTOM_ZONE" :: Text)
    ]


indexFilter :: Text -> Value
indexFilter val =
  object
    [ "comparator" .= ("EQUALS" :: Text)
    , "fieldName" .= ("indexName" :: Text)
    , "fieldValue" .= object ["type" .= ("STRING" :: Text), "value" .= val]
    ]


addNotesHeaders :: Request -> Request
addNotesHeaders req = req{requestHeaders = notesHeaders <> requestHeaders req}


notesHeaders :: [(HeaderName, BS8.ByteString)]
notesHeaders =
  [ (hAccept, "application/json")
  , (hUserAgent, "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36")
  , (mk "Origin", icloudHome)
  , (hReferer, icloudHome <> "/")
  ]


icloudHome :: BS8.ByteString
icloudHome = "https://www.icloud.com"


stripTrailingSlash :: BS8.ByteString -> BS8.ByteString
stripTrailingSlash bs
  | not (BS8.null bs) && BS8.last bs == '/' = BS8.init bs
  | otherwise = bs


lookupAndParse :: Text -> Map Text Text -> IO Request
lookupAndParse key ws =
  case Map.lookup key ws of
    Nothing -> fail $ "icloud-notes: webservice URL not found for key: " <> Text.unpack key
    Just url -> parseRequest (Text.unpack url)
