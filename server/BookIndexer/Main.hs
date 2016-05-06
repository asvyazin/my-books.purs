{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where


import BookIndexer.CouchDB.Changes.Watcher (watchChanges, WatchParams(..), DocumentChange(..))
import Common.JSONHelper (jsonParser)
import Common.ServerEnvironmentInfo (getServerEnvironment, ServerEnvironmentInfo(..))
import Common.UserInfo (UserInfo)
import Control.Concurrent.Async (withAsync, wait)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow(throwM), MonadCatch(catch))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), ToJSON(toJSON), (.=), object)
import Data.Conduit (($$), (=$=))
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Conduit.Combinators as DC (last, mapM_, mapM)
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


watchNewUsersLoop :: (MonadReader env m, HasHttpManager env, MonadIO m) => Text -> Maybe Int64 -> m (Maybe Int64)
watchNewUsersLoop couchdbServer lastSeq = do
  httpEnv <- ask
  let
    watchParams =
      WatchParams
      { _baseUrl = couchdbServer
      , _database = usersDatabaseName
      , _since = lastSeq
      , _filter = Just usersFilter
      }
    watchNewUsersLoop' = do
      liftIO $ putStrLn "starting watching"
      watchChanges watchParams =$= DC.mapM (getUserInfo couchdbServer) =$= DC.mapM_ processUser $$ DC.last
  liftIO $ withAsync (runReaderT (runResourceT watchNewUsersLoop') httpEnv) wait


usersFilter :: Text
usersFilter =
  "users/all"


getUserInfo :: (MonadIO m, MonadReader env m, HasHttpManager env, MonadThrow m, MonadBaseControl IO m) => Text -> DocumentChange -> m UserInfo
getUserInfo couchdbUrl documentChange = do
  liftIO $ print documentChange
  let
    docId = _id documentChange
  req <- parseRequest $ unpack $ couchdbUrl <> "/" <> usersDatabaseName <> "/" <> docId
  withResponse req $ \resp ->
    responseBody resp $$ sinkParser jsonParser
  

processUser :: (MonadIO m) => UserInfo -> m ()
processUser userInfo =
  liftIO $ print userInfo
