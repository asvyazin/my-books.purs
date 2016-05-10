{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where


import BookIndexer.CouchDB.Changes.Watcher (watchChanges, WatchParams(..), DocumentChange, _seq)
import qualified BookIndexer.CouchDB.Changes.Watcher as W (_id)
import BookIndexer.Environment (Environment(Environment), manager, serverEnvironment)
import BookIndexer.IndexerState (IndexerState(IndexerState), rev, lastSeq, indexerStateId)
import BookIndexer.Onedrive.FolderChangesReader (FolderChangesReader, newFolderChangesReader, enumerateChanges)
import Common.BooksDirectoryInfo (getBooksDirectoryInfo, booksItemId)
import Common.Database (usersDatabaseName, indexerDatabaseName, userDatabaseName, usersFilter)
import Common.HTTPHelper (textUrlEncode)
import Common.JSONHelper (jsonParser)
import Common.OnedriveInfo (OnedriveInfo, onedriveInfoId, getOnedriveInfo, token)
import Common.ServerEnvironmentInfo (getServerEnvironment, ServerEnvironmentInfo, couchdbServer)
import Common.UserInfo (UserInfo)
import qualified Common.UserInfo as U (_id)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, modifyTVar, TVar)
import Control.Lens ((^.), view, set)
import Control.Monad (void, (=<<), when)
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Catch (MonadThrow(throwM), MonadCatch(catch))
import Control.Monad.IO.Class (MonadIO(liftIO))
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
import Network.HTTP.Types.Status (notFound404, unauthorized401)
import Network.HTTP.Types.URI (urlEncode)


type UserSynchronizers =
  TVar [Async ()]


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
      ls = view lastSeq <$> state
    maybeNewLastSeq <- watchNewUsersLoop userSynchronizers ls
    case maybeNewLastSeq of
      Nothing ->
        return ()
      Just newLastSeq ->
        setIndexerState $ newIndexerState state newLastSeq


getIndexerState :: (MonadCatch m, MonadIO m, MonadReader Environment m, MonadBaseControl IO m) => m (Maybe IndexerState)
getIndexerState = do
  req <- parseRequest =<< getObjectUrl indexerDatabaseName indexerStateId
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
  initReq <- parseRequest =<< getObjectUrl indexerDatabaseName indexerStateId
  let
    req =
      setRequestMethod "PUT" $ setRequestBodyJSON indexerState initReq
  void $ httpNoBody req


newIndexerState :: Maybe IndexerState -> Int64 -> IndexerState
newIndexerState current ls =
  maybe (IndexerState Nothing ls) (set lastSeq ls) current

  
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


synchronizeUserLoop :: (MonadBase IO m, MonadCatch m, MonadIO m, MonadReader Environment m, MonadBaseControl IO m) => UserInfo -> m ()
synchronizeUserLoop userInfo = do
  couchdbUrl <- view (serverEnvironment . couchdbServer)
  let
    userDatabaseId =
      userDatabaseName $ userInfo ^. U._id
  onedriveInfo <- getOnedriveInfo couchdbUrl userDatabaseId
  liftBase $ print onedriveInfo
  booksDirectoryInfo <- getBooksDirectoryInfo couchdbUrl userDatabaseId
  liftBase $ print booksDirectoryInfo
  onedriveReader <- newFolderChangesReader (onedriveInfo ^. token) (booksDirectoryInfo ^. booksItemId) Nothing
  (enumerateChanges onedriveReader $$ DC.mapM_ (liftIO . print)) `catch` handleError
  where
    handleError :: (MonadThrow m, MonadIO m) => HttpException -> m ()
    handleError e =
      liftIO $ print e -- TODO proper handling, use reauthorizeLoop below


reauthorizeLoop :: (MonadCatch m) => (a -> m ()) -> (a -> m a) -> a -> m ()
reauthorizeLoop worker reauthorizeHandler context = do
  needReauthorize <- iteration
  when needReauthorize $ do
    newContext <- reauthorizeHandler context
    reauthorizeLoop worker reauthorizeHandler context
  where
    iteration =
      (worker context >> return False) `catch` handleError
    handleError :: (MonadThrow m) => HttpException -> m Bool
    handleError e@(StatusCodeException statusCode _ _ )
      | statusCode == unauthorized401 =
        return True
      | otherwise =
        throwM e
    handleError e =
      throwM e
