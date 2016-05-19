{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module BookIndexer.CouchDB.Changes.Watcher where


import Common.JSONHelper (jsonParser)
import Control.Applicative ((<|>))
import Control.Lens (makeLenses)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Blaze.ByteString.Builder (toByteString)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.ByteString.Char8 (pack, unpack)
import Data.Conduit ((=$=), Sink)
import Data.Conduit.Attoparsec (conduitParser)
import qualified Data.Conduit.Combinators as DC (map)
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.URI (renderQuery, encodePathSegments)
import Network.HTTP.Simple (parseRequest, httpSink)


watchChanges :: (MonadIO m, MonadMask m) => WatchParams -> Sink WatchItem m a -> m a
watchChanges params consumer = do
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

  req <- parseRequest $ unpack url
  httpSink req $ const $ conduitParser watchItemParser =$= DC.map snd =$= consumer


data WatchParams =
  WatchParams
  { _baseUrl :: Text
  , _database :: Text
  , _since :: Maybe Int64
  , _filter :: Maybe Text
  }


data WatchItem
  = DocumentChangeItem DocumentChange
  | LastSeqItem LastSeq
  deriving (Show)


data DocumentChange =
  DocumentChange
  { __seq :: Int64
  , __id :: Text
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


data LastSeq =
  LastSeq
  { _lastSeq :: Int64
  } deriving (Show)


instance FromJSON LastSeq where
  parseJSON (Object o) =
    LastSeq <$> o .: "last_seq"
  parseJSON _ =
    error "Invalid LastSeq JSON"


watchItemParser :: Parser WatchItem
watchItemParser =
  (DocumentChangeItem <$> jsonParser) <|> (LastSeqItem <$> jsonParser)


makeLenses ''DocumentChange
makeLenses ''Change
makeLenses ''LastSeq
