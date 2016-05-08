{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where


import BookIndexer.CouchDB.Changes.Watcher (watchChanges, WatchParams(..), DocumentChange(..))
import Common.JSONHelper (jsonParser)
import Common.ServerEnvironmentInfo (getServerEnvironment, ServerEnvironmentInfo(..))
import Common.UserInfo (UserInfo)
import Control.Monad (void)
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Catch (MonadThrow(throwM), MonadCatch(catch))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), ToJSON(toJSON), (.=), object)
import Data.Conduit (($$), (=$=), Sink, passthroughSink)
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Conduit.Combinators as DC (last, map, mapM, mapM_)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Network.HTTP.Client.Conduit (withResponse, responseBody, HasHttpManager, withManager, httpNoBody, HttpException(StatusCodeException))
import Network.HTTP.Simple (setRequestMethod, setRequestBodyJSON, parseRequest)
import Network.HTTP.Types.Status (notFound404)


main :: IO ()
main = do
  serverEnvironment <- getServerEnvironment
  let
    couchdbServer =
      _couchdbServer serverEnvironment
  withManager $ do
    state <- getIndexerState couchdbServer
    let
      lastSeq = _lastSeq <$> state
    maybeNewLastSeq <- watchNewUsersLoop couchdbServer lastSeq
    case maybeNewLastSeq of
      Nothing ->
        return ()
      Just newLastSeq ->
        setIndexerState couchdbServer $ newIndexerState state newLastSeq


getIndexerState :: (MonadCatch m, MonadIO m, MonadReader env m, HasHttpManager env, MonadBaseControl IO m) => Text -> m (Maybe IndexerState)
getIndexerState couchdbUrl = do
  req <- parseRequest $ unpack $ couchdbUrl <> "/" <> indexerDatabaseName <> "/" <> indexerStateId
  withResponse req processResponse `catch` handleError
  where
    processResponse resp =
      responseBody resp $$ sinkParser jsonParser
    
    handleError :: (MonadThrow m) => HttpException -> m (Maybe IndexerState)
    handleError e@(StatusCodeException code _ _) =
      if code == notFound404
      then return Nothing
      else throwM e
    handleError e = throwM e


setIndexerState :: (MonadThrow m, MonadIO m, MonadReader env m, HasHttpManager env) => Text -> IndexerState -> m ()
setIndexerState couchdbUrl indexerState = do
  initReq <- parseRequest $ unpack $ couchdbUrl <> "/" <> indexerDatabaseName <> "/" <> indexerStateId
  let
    req =
      setRequestMethod "PUT" $ setRequestBodyJSON indexerState initReq
  void $ httpNoBody req


indexerStateId :: Text
indexerStateId =
  "indexerState"


indexerDatabaseName :: Text
indexerDatabaseName =
  "my-books-indexer"


usersDatabaseName :: Text
usersDatabaseName =
  "my-books"


data IndexerState =
  IndexerState
  { __rev :: Maybe String
  , _lastSeq :: Int64
  }


instance FromJSON IndexerState where
  parseJSON (Object v) =
    IndexerState <$> v .: "_rev" <*> v .: "last_seq"
  parseJSON _ =
    error "Invalid IndexerState JSON"


instance ToJSON IndexerState where
  toJSON indexerState =
    let 
      state =
        [ Just ("_id" .= indexerStateId)
        , formatRev <$> __rev indexerState
        , Just ("last_seq" .= _lastSeq indexerState)
        ]
      
      formatRev str =
        "_rev" .= str
    in
      object $ catMaybes state


newIndexerState :: Maybe IndexerState -> Int64 -> IndexerState
newIndexerState current lastSeq =
  maybe (IndexerState Nothing lastSeq) (\s -> s { _lastSeq = lastSeq }) current


watchNewUsersLoop :: (MonadReader env m, HasHttpManager env, MonadThrow m, MonadIO m, MonadBase IO m, MonadBaseControl IO m) => Text -> Maybe Int64 -> m (Maybe Int64)
watchNewUsersLoop couchdbServer lastSeq = do
  let
    watchParams =
      WatchParams
      { _baseUrl = couchdbServer
      , _database = usersDatabaseName
      , _since = lastSeq
      , _filter = Just usersFilter
      }
    watchNewUsersLoop' =
      watchChanges watchParams =$= passthroughSink (processDocumentChange couchdbServer) return =$= DC.map _seq $$ DC.last -- TODO: catch exceptions
  runResourceT watchNewUsersLoop'


usersFilter :: Text
usersFilter =
  "users/all"


processDocumentChange :: (MonadIO m, MonadReader env m, HasHttpManager env, MonadThrow m, MonadBaseControl IO m) => Text -> Sink DocumentChange m ()
processDocumentChange couchdbServer =
  DC.mapM (getUserInfo couchdbServer) =$= DC.mapM_ processUser


getUserInfo :: (MonadIO m, MonadBase IO m, MonadReader env m, HasHttpManager env, MonadThrow m, MonadBaseControl IO m) => Text -> DocumentChange -> m UserInfo
getUserInfo couchdbServer documentChange = do
  liftBase $ print documentChange
  let
    docId = _id documentChange
  req <- parseRequest $ unpack $ couchdbServer <> "/" <> usersDatabaseName <> "/" <> docId
  withResponse req $ \resp ->
    responseBody resp $$ sinkParser jsonParser
  

processUser :: (MonadBase IO m) => UserInfo -> m ()
processUser userInfo =
  liftBase $ print userInfo
