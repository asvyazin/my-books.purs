{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where


import BookIndexer.CouchDB.Changes.Watcher (watchChanges, WatchParams(..), DocumentChange, WatchItem(..), LastSeq(..))
import qualified BookIndexer.CouchDB.Changes.Watcher as W (_id)
import BookIndexer.IndexerState (IndexerState(IndexerState), lastSeq, indexerStateId)
import BookIndexer.Onedrive.FolderChangesReader (newFolderChangesReader, enumerateChanges, getCurrentEnumerationToken)
import Common.BooksDirectoryInfo (getBooksDirectoryInfo, booksItemId)
import Common.Database (usersDatabaseName, indexerDatabaseName, userDatabaseName, usersFilter)
import qualified Common.Onedrive as OD (oauthRefreshTokenRequest, OauthTokenRequest(..), OauthTokenResponse(..), getOnedriveClientSecret)
import Common.OnedriveInfo (getOnedriveInfo, token, refreshToken)
import Common.ServerEnvironmentInfo (getServerEnvironment, couchdbServer, onedriveClientId, appBaseUrl, ServerEnvironmentInfo)
import Common.UserInfo (UserInfo)
import qualified Common.UserInfo as U (_id)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, modifyTVar, TVar)
import Control.Exception.Base (SomeException)
import Control.Lens ((^.), view, set)
import Control.Monad (void, when)
import Control.Monad.Catch (MonadThrow(throwM), MonadCatch(catch), MonadMask)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Conduit (($$), (=$=))
import qualified Data.Conduit.Combinators as DC (last, mapM_, concatMapM)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Network.HTTP.Client.Conduit (HttpException(StatusCodeException))
import Network.HTTP.Simple (setRequestMethod, setRequestBodyJSON, parseRequest, httpJSON, getResponseBody, httpLBS)
import Network.HTTP.Types.Status (notFound404, unauthorized401)


type UserSynchronizers =
  TVar [Async ()]


main :: IO ()
main = do
  serverEnv <- getServerEnvironment
  userSynchronizers <- atomically $ newTVar []
  runReaderT (runMain  userSynchronizers) serverEnv
  where
    runMain userSynchronizers = do
      state <- getIndexerState
      let
        ls = view lastSeq <$> state
      maybeNewLastSeq <- watchNewUsersLoop userSynchronizers ls
      case maybeNewLastSeq of
        Nothing ->
          return ()
        Just newLastSeq ->
          setIndexerState $ newIndexerState state newLastSeq


getIndexerState :: (MonadCatch m, MonadIO m, MonadReader ServerEnvironmentInfo m) => m (Maybe IndexerState)
getIndexerState = do
  req <- parseRequest =<< getObjectUrl indexerDatabaseName indexerStateId
  (getResponseBody <$> httpJSON req) `catch` handleError
  where
    handleError :: (MonadThrow m) => HttpException -> m (Maybe IndexerState)
    handleError e@(StatusCodeException code _ _) =
      if code == notFound404
      then return Nothing
      else throwM e
    handleError e = throwM e


setIndexerState :: (MonadThrow m, MonadIO m, MonadReader ServerEnvironmentInfo m) => IndexerState -> m ()
setIndexerState indexerState = do
  initReq <- parseRequest =<< getObjectUrl indexerDatabaseName indexerStateId
  let
    req =
      setRequestMethod "PUT" $ setRequestBodyJSON indexerState initReq
  void $ httpLBS req


newIndexerState :: Maybe IndexerState -> Int64 -> IndexerState
newIndexerState current ls =
  maybe (IndexerState Nothing ls) (set lastSeq ls) current

  
watchNewUsersLoop :: (MonadReader ServerEnvironmentInfo m, MonadMask  m, MonadIO m) => UserSynchronizers -> Maybe Int64 -> m (Maybe Int64)
watchNewUsersLoop userSynchronizers ls = do
  couchdbUrl <- view couchdbServer
  let
    watchParams =
      WatchParams
      { _baseUrl = couchdbUrl
      , _database = usersDatabaseName
      , _since = ls
      , _filter = Just usersFilter
      }
  watchChanges watchParams (DC.concatMapM (processWatchItem userSynchronizers) =$= DC.last) -- TODO: catch exceptions


processWatchItem :: (MonadIO m, MonadReader ServerEnvironmentInfo m, MonadThrow m) => UserSynchronizers -> WatchItem -> m (Maybe Int64)
processWatchItem _ (LastSeqItem (LastSeq ls)) =
  return $ Just ls
processWatchItem userSynchronizers (DocumentChangeItem documentChange) =
  getUserInfo documentChange >>= processUser userSynchronizers >> return Nothing


getUserInfo :: (MonadIO m, MonadThrow m, MonadReader ServerEnvironmentInfo m) => DocumentChange -> m UserInfo
getUserInfo documentChange = do
  let
    docId = documentChange ^. W._id
  req <- parseRequest =<< getObjectUrl usersDatabaseName docId
  resp <- httpJSON req
  return $ getResponseBody resp


getObjectUrl :: MonadReader ServerEnvironmentInfo m => Text -> Text -> m String
getObjectUrl databaseId objectId = do
  couchdbUrl <- view couchdbServer
  return $ unpack $ couchdbUrl <> "/" <> databaseId <> "/" <> objectId
  

processUser :: (MonadIO m, MonadReader ServerEnvironmentInfo m) => UserSynchronizers -> UserInfo -> m ()
processUser userSynchronizers userInfo = do
  env <- ask
  synchronizer <- liftIO $ async $ runReaderT (synchronizeUserLoop userInfo) env
  liftIO $ atomically $ modifyTVar userSynchronizers (synchronizer :)
  return ()


synchronizeUserLoop :: (MonadCatch m, MonadIO m, MonadReader ServerEnvironmentInfo m, MonadBaseControl IO m) => UserInfo -> m ()
synchronizeUserLoop userInfo = do
  couchdbUrl <- view couchdbServer
  let
    userDatabaseId =
      userDatabaseName $ userInfo ^. U._id
  onedriveInfo <- getOnedriveInfo couchdbUrl userDatabaseId
  booksDirectoryInfo <- getBooksDirectoryInfo couchdbUrl userDatabaseId
  onedriveReader <- newFolderChangesReader (onedriveInfo ^. token) (booksDirectoryInfo ^. booksItemId) Nothing
  reauthorizeLoop readChanges doRefreshToken onedriveReader `catch` logError
  where
    readChanges reader =
      enumerateChanges reader $$ DC.mapM_ processItem
    doRefreshToken oldReader = do
      secret <- liftIO OD.getOnedriveClientSecret
      env <- ask
      let
        couchdbUrl =
          env ^. couchdbServer
        userDatabaseId =
          userDatabaseName $ userInfo ^. U._id
      onedriveInfo <- getOnedriveInfo couchdbUrl userDatabaseId
      let
        tok =
          fromJust (onedriveInfo ^. refreshToken)
        req =
          OD.OauthTokenRequest
          { OD.clientId = env ^. onedriveClientId
          , OD.redirectUri = (env ^. appBaseUrl) <> "/onedrive-redirect"
          , OD.clientSecret = secret
          }
      resp <- OD.oauthRefreshTokenRequest req tok
      -- TODO update onedriveInfo in DB
      currentEnumerationToken <- getCurrentEnumerationToken oldReader
      booksDirectoryInfo <- getBooksDirectoryInfo couchdbUrl userDatabaseId
      newFolderChangesReader (OD.accessToken resp) (booksDirectoryInfo ^. booksItemId) currentEnumerationToken
    logError :: (MonadThrow m, MonadIO m) => SomeException -> m a
    logError e = do
      liftIO $ print e
      throwM e
    processItem _ =
      return ()


reauthorizeLoop :: (MonadCatch m) => (a -> m ()) -> (a -> m a) -> a -> m ()
reauthorizeLoop worker reauthorizeHandler context = do
  needReauthorize <- iteration
  when needReauthorize $ do
    newContext <- reauthorizeHandler context
    reauthorizeLoop worker reauthorizeHandler newContext
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
