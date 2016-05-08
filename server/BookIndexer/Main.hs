{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where


import BookIndexer.CouchDB.Changes.Watcher (watchChanges, WatchParams(..), DocumentChange, _seq)
import qualified BookIndexer.CouchDB.Changes.Watcher as W (_id)
import Common.JSONHelper (jsonParser)
import Common.OnedriveInfo (OnedriveInfo, onedriveInfoId)
import Common.ServerEnvironmentInfo (getServerEnvironment, ServerEnvironmentInfo, couchdbServer)
import Common.UserInfo (UserInfo)
import qualified Common.UserInfo as U (_id)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, modifyTVar, TVar)
import Control.Lens ((^.), makeLenses, view)
import Control.Monad (void, (=<<))
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Catch (MonadThrow(throwM), MonadCatch(catch))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT, withReaderT)
import Control.Monad.Reader.Class (MonadReader(ask))
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
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Client.Conduit (withResponse, responseBody, HasHttpManager(getHttpManager), withManager, httpNoBody, HttpException(StatusCodeException), Manager)
import Network.HTTP.Simple (setRequestMethod, setRequestBodyJSON, parseRequest)
import Network.HTTP.Types.Status (notFound404)
import Network.HTTP.Types.URI (urlEncode)


type UserSynchronizers =
  TVar [Async ()]


data Environment =
  Environment
  { _httpManager :: Manager
  , _serverEnvironment :: ServerEnvironmentInfo
  }


makeLenses ''Environment


instance HasHttpManager Environment where
  getHttpManager = _httpManager


main :: IO ()
main = do
  serverEnv <- getServerEnvironment
  let
    convertEnvironment mgr =
      Environment mgr serverEnv
  userSynchronizers <- atomically $ newTVar []
  withManager $ withReaderT convertEnvironment $ do
    state <- getIndexerState
    let
      lastSeq = _lastSeq <$> state
    maybeNewLastSeq <- watchNewUsersLoop userSynchronizers lastSeq
    case maybeNewLastSeq of
      Nothing ->
        return ()
      Just newLastSeq ->
        setIndexerState $ newIndexerState state newLastSeq


getIndexerState :: (MonadCatch m, MonadIO m, MonadReader Environment m, MonadBaseControl IO m) => m (Maybe IndexerState)
getIndexerState = do
  req <- parseRequest =<< indexerStateUrl
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


setIndexerState :: (MonadThrow m, MonadIO m, MonadReader Environment m) => IndexerState -> m ()
setIndexerState indexerState = do
  initReq <- parseRequest =<< indexerStateUrl
  let
    req =
      setRequestMethod "PUT" $ setRequestBodyJSON indexerState initReq
  void $ httpNoBody req


indexerStateUrl :: MonadReader Environment m => m String
indexerStateUrl = do
  couchdbUrl <- view (serverEnvironment . couchdbServer)
  return $ unpack $ couchdbUrl <> "/" <> indexerDatabaseName <> "/" <> indexerStateId


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

  
watchNewUsersLoop :: (MonadReader Environment m, MonadThrow m, MonadIO m, MonadBase IO m, MonadBaseControl IO m) => UserSynchronizers -> Maybe Int64 -> m (Maybe Int64)
watchNewUsersLoop userSynchronizers lastSeq = do
  couchdbUrl <- view (serverEnvironment . couchdbServer)
  let
    watchParams =
      WatchParams
      { _baseUrl = couchdbUrl
      , _database = usersDatabaseName
      , _since = lastSeq
      , _filter = Just usersFilter
      }
    watchNewUsersLoop' =
      watchChanges watchParams =$= passthroughSink (processDocumentChange userSynchronizers) return =$= DC.map (^. _seq) $$ DC.last -- TODO: catch exceptions
  runResourceT watchNewUsersLoop'


usersFilter :: Text
usersFilter =
  "users/all"


processDocumentChange :: (MonadIO m, MonadReader Environment m, MonadThrow m, MonadBaseControl IO m) => UserSynchronizers -> Sink DocumentChange m ()
processDocumentChange userSynchronizers =
  DC.mapM getUserInfo =$= DC.mapM_ (processUser userSynchronizers)


getUserInfo :: (MonadIO m, MonadBase IO m, MonadReader Environment m, MonadThrow m, MonadBaseControl IO m) => DocumentChange -> m UserInfo
getUserInfo documentChange = do
  liftBase $ print documentChange
  let
    docId = documentChange ^. W._id
  req <- parseRequest =<< getObjectUrl usersDatabaseName docId
  withResponse req $ \resp ->
    responseBody resp $$ sinkParser jsonParser


getObjectUrl :: MonadReader Environment m => Text -> Text -> m String
getObjectUrl databaseId objectId = do
  couchdbUrl <- view (serverEnvironment . couchdbServer)
  return $ unpack $ couchdbUrl <> "/" <> databaseId <> "/" <> objectId
  

processUser :: (MonadBase IO m, MonadReader Environment m) => UserSynchronizers -> UserInfo -> m ()
processUser userSynchronizers userInfo = do
  env <- ask
  liftBase $ print userInfo
  synchronizer <- liftBase $ async $ runReaderT (synchronizeUserLoop userInfo) env
  liftBase $ atomically $ modifyTVar userSynchronizers (synchronizer :)


synchronizeUserLoop :: (MonadBase IO m, MonadThrow m, MonadIO m, MonadReader Environment m, MonadBaseControl IO m) => UserInfo -> m ()
synchronizeUserLoop userInfo = do
  onedriveInfo <- getOnedriveInfo (userInfo ^. U._id)
  liftBase $ print onedriveInfo


getOnedriveInfo :: (MonadThrow m, MonadIO m, MonadReader Environment m, MonadBaseControl IO m) => Text -> m OnedriveInfo
getOnedriveInfo userId = do
  req <- parseRequest =<< getObjectUrl (userDatabaseName userId) onedriveInfoId
  withResponse req $ \resp ->
    responseBody resp $$ sinkParser jsonParser


userDatabaseName :: Text -> Text
userDatabaseName userId =
  textUrlEncode $ usersDatabaseName <> "/" <> userId


textUrlEncode :: Text -> Text
textUrlEncode =
  decodeUtf8 . urlEncode False . encodeUtf8
