{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module BookIndexer.CouchDB.Changes.Watcher where


import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Blaze.ByteString.Builder (toByteString)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?), json', fromJSON, Result(Success, Error))
import Data.Attoparsec.ByteString.Char8 (Parser, endOfLine)
import Data.ByteString.Char8 (pack, unpack)
import Data.Conduit (Source, (=$=))
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.List as CL (map)
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.URI (renderQuery, encodePathSegments)
import Network.HTTP.Client.Conduit (parseUrl, responseBody, HasHttpManager, responseOpen)


watchChanges :: (MonadThrow m, MonadReader env m, HasHttpManager env, MonadIO m) => WatchParams -> Source m DocumentChange
watchChanges params = do
  let
    sinceParam i64 =
      ("since", Just $ pack $ show i64)

    filterParam flt =
      ("filter", Just $ encodeUtf8 flt)
    
    qsParams =
      [ Just ("feed", Just "continuous")
      , sinceParam <$> _since params
      , filterParam <$> _filter params
      ]

    qs =
      renderQuery True $ catMaybes qsParams

    url =
      encodeUtf8 (_baseUrl params) <> toByteString (encodePathSegments [_database params, "_changes"]) <> qs

  req <- parseUrl $ unpack url
  resp <- responseOpen req
  responseBody resp =$= conduitParser changesParser =$= CL.map snd


data WatchParams =
  WatchParams
  { _baseUrl :: Text
  , _database :: Text
  , _since :: Maybe Int64
  , _filter :: Maybe Text
  }


data DocumentChange =
  DocumentChange
  { _seq :: Int64
  , _id :: Text
  , _changes :: [Change]
  } deriving (Show)


data Change =
  Change
  { _rev :: Text
  , _deleted :: Bool
  } deriving (Show)


instance FromJSON DocumentChange where
  parseJSON (Object v) =
    DocumentChange <$> (v .: "seq") <*> (v .: "id") <*> (v .: "changes")
  parseJSON _ =
    error "Invalid DocumentChange JSON"


instance FromJSON Change where
  parseJSON (Object v) = do
    rev <- v .: "rev"
    deleted <- fromMaybe False <$> v .:? "deleted"
    return $ Change rev deleted
  parseJSON _ =
    error "Invalid Change JSON"


changesParser :: Parser DocumentChange
changesParser = do
  v <- json' <* endOfLine
  case fromJSON v of
    Success r ->
      return r
    Error e ->
      fail e
