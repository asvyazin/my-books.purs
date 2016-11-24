{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where


import qualified BookIndexer.BookMetadataReader as BM (loadMetadata, BookMetadata(author, title, epubVersion))
import BookIndexer.IndexerState (IndexerState(IndexerState), lastSeq, indexerStateId)
import BookIndexer.Types.BookInfo (BookInfo(BookInfo), read_, author, title, epubVersion, onedriveId)
import qualified BookIndexer.Types.BookInfo as BI (token)
import Common.BooksDirectoryInfo (BooksDirectoryInfo, booksDirectoryInfoId, booksItemId, readItemId, defaultBooksDirectoryInfo)
import Common.Database (usersDatabaseName, indexerDatabaseName, userDatabaseName, usersFilter)
import qualified Common.Onedrive as OD (getOnedriveClientSecret)
import Common.OnedriveInfo (onedriveInfoId, defaultOnedriveInfo, token, refreshToken)
import Common.ServerEnvironmentInfo (getServerEnvironment, couchdbServer, onedriveClientId, appBaseUrl, ServerEnvironmentInfo)
import Common.UserInfo (UserInfo)
import qualified Common.UserInfo as U (_id)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, modifyTVar, TVar)
import Control.Error.Util (hoistMaybe)
import Control.Exception.Base (SomeException)
import Control.Lens ((^.), view, set, _Just)
import Control.Monad (void, unless, when)
import Control.Monad.Catch (MonadThrow(throwM), MonadCatch(catch), MonadMask)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.State (runStateT)
import Control.Monad.State.Class (MonadState(get, put))
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(MaybeT))
import CouchDB.Changes.Watcher (watchChanges, WatchParams(..), DocumentChange)
import qualified CouchDB.Changes.Watcher as W (_id)
import CouchDB.Requests (getObject, putObject)
import CouchDB.Types.Auth (Auth(NoAuth, BasicAuth))
import CouchDB.Types.Seq (Seq)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Conduit (($$), (=$=))
import qualified Data.Conduit.Combinators as DC (last, mapM_, concatMapM)
import Data.Maybe (fromJust, isNothing, maybe, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as S (Set, member, insert, singleton)
import Data.Text (Text)
import Onedrive.Auth (requestRefreshToken)
import Onedrive.FolderChangesReader (newFolderChangesReader, enumerateChanges)
import Onedrive.Session (newSessionWithRenewableToken, Session)
import qualified Onedrive.Types.ItemReference as IR (id_)
import Onedrive.Types.OnedriveItem (name, id_, parentReference, OnedriveItem, folder)
import Onedrive.Types.OauthTokenRequest (OauthTokenRequest(OauthTokenRequest))
import Onedrive.Types.OauthTokenResponse (accessToken)
import System.Environment (lookupEnv)


type UserSynchronizers =
  TVar [Async ()]


main :: IO ()
main = do
  serverEnv <- getServerEnvironment
  let couchdb = serverEnv ^. couchdbServer
  userSynchronizers <- atomically $ newTVar []
  auth <- getAuth
  state <- getObject couchdb indexerDatabaseName auth indexerStateId
  let
    ls = view lastSeq <$> state
  maybeNewLastSeq <- runReaderT (watchNewUsersLoop userSynchronizers auth ls) serverEnv
  case maybeNewLastSeq of
    Nothing ->
      return ()
    Just newLastSeq ->
      putObject couchdb indexerDatabaseName auth indexerStateId $ newIndexerState state newLastSeq


newIndexerState :: Maybe IndexerState -> Seq -> IndexerState
newIndexerState current ls =
  maybe (IndexerState Nothing ls) (set lastSeq ls) current


watchNewUsersLoop :: (MonadMask  m
                     , MonadReader ServerEnvironmentInfo m
                     , MonadIO m) => UserSynchronizers -> Auth -> Maybe Seq -> m (Maybe Seq)
watchNewUsersLoop userSynchronizers auth ls = do
  env <- ask
  let
    watchParams =
      WatchParams
      { _baseUrl = env ^. couchdbServer
      , _database = usersDatabaseName
      , _since = ls
      , _filter = Just usersFilter
      }
  watchChanges auth watchParams (DC.concatMapM (processWatchItem userSynchronizers auth) =$= DC.last) -- TODO: catch exceptions


processWatchItem :: (MonadIO m
                    , MonadReader ServerEnvironmentInfo m
                    , MonadThrow m) => UserSynchronizers -> Auth -> DocumentChange -> m (Maybe Seq)
processWatchItem userSynchronizers auth documentChange = runMaybeT $ do
  ui <- MaybeT $ getUserInfo auth documentChange
  processUser userSynchronizers auth ui
  hoistMaybe Nothing


getUserInfo :: (MonadIO m, MonadThrow m, MonadReader ServerEnvironmentInfo m) => Auth -> DocumentChange -> m (Maybe UserInfo)
getUserInfo auth documentChange = do
  env <- ask
  let
    couchdb = env ^. couchdbServer
    docId = documentChange ^. W._id
  getObject couchdb usersDatabaseName auth docId


processUser :: (MonadIO m, MonadReader ServerEnvironmentInfo m) => UserSynchronizers -> Auth -> UserInfo -> m ()
processUser userSynchronizers auth userInfo = do
  env <- ask
  synchronizer <- liftIO $ async $ runReaderT (synchronizeUserLoop auth userInfo) env
  liftIO $ atomically $ modifyTVar userSynchronizers (synchronizer :)
  return ()


synchronizeUserLoop :: (MonadCatch m, MonadIO m, MonadReader ServerEnvironmentInfo m) => Auth -> UserInfo -> m ()
synchronizeUserLoop auth userInfo = do
  serverEnv <- ask
  let
    couchdb =
      serverEnv ^. couchdbServer
    userDatabaseId =
      userDatabaseName $ userInfo ^. U._id
  onedriveInfo <- fromMaybe defaultOnedriveInfo <$> getObject couchdb userDatabaseId auth onedriveInfoId
  booksDirectoryInfo <- fromMaybe defaultBooksDirectoryInfo <$> getObject couchdb userDatabaseId auth booksDirectoryInfoId
  let
    tok =
      onedriveInfo ^. token
    refreshTok =
      fromJust $ onedriveInfo ^. refreshToken
    booksFolder =
      booksDirectoryInfo ^. booksItemId
    readFolder =
      booksDirectoryInfo ^. readItemId
  liftIO $ putStrLn $ "Token: " ++ show tok
  liftIO $ putStrLn $ "Refresh token: " ++ show refreshTok
  liftIO $ putStrLn $ "Books itemId: " ++ show booksFolder
  session <- liftIO $ newSessionWithRenewableToken tok (runReaderT (renewToken refreshTok) serverEnv)
  onedriveReader <- newFolderChangesReader session booksFolder Nothing
  let
    loop =
      enumerateChanges onedriveReader $$ DC.mapM_ (processItem session couchdb booksDirectoryInfo)
    startState =
      (S.singleton readFolder, S.singleton booksFolder)
  void (runStateT loop startState) `catch` logError
  where
    renewToken tok = do
      secret <- liftIO OD.getOnedriveClientSecret
      env <- ask
      let
        req =
          OauthTokenRequest (env ^. onedriveClientId) ((env ^. appBaseUrl) <> "/onedrive-redirect") secret
      resp <- requestRefreshToken req tok
      return $ resp ^. accessToken

    logError :: (MonadThrow m, MonadIO m) => SomeException -> m a
    logError e = do
      liftIO $ print e
      throwM e

    processItem :: (MonadThrow m
                   , MonadIO m
                   , MonadState (S.Set Text, S.Set Text) m) => Session -> Text -> BooksDirectoryInfo -> OnedriveItem -> m ()
    processItem session couchdb booksDirectoryInfo i = do
      let
        currentId =
          i ^. id_

        filename =
          i ^. name

        parentItemId =
          i ^. parentReference . _Just . IR.id_

        isFile =
          isNothing $ i ^. folder

        booksFolder =
          booksDirectoryInfo ^. booksItemId

        readFolder =
          booksDirectoryInfo ^. readItemId

        bookInfoToken =
          booksFolder <> ":" <> readFolder

        bookInfoId =
          "books/" <> currentId

        userDatabaseId =
          userDatabaseName $ userInfo ^. U._id

        createOrUpdateBookInfo itemId isRead bookMetadata = do
          let
            a =
              BM.author <$> bookMetadata
            t =
              BM.title <$> bookMetadata
            v =
              BM.epubVersion <$> bookMetadata
            defaultBook =
              BookInfo bookInfoId Nothing isRead bookInfoToken a t v (Just currentId)
            updateBook =
              set BI.token bookInfoToken . set read_ isRead . set author a . set title t . set epubVersion v . set onedriveId (Just currentId)
          newBook <- maybe defaultBook updateBook <$> getObject couchdb userDatabaseId auth itemId
          putObject couchdb userDatabaseId auth itemId newBook

      (readSet, processedSet) <- get
      unless (S.member currentId processedSet) $ do
        unless (S.member parentItemId processedSet) $
          liftIO $ putStrLn $ "Parent itemId not found: " ++ show parentItemId
        when isFile $ do
          let
            isRead =
              S.member parentItemId readSet
            prefix =
              if isRead then "Read" else "Not read"
          result <- runExceptT $ BM.loadMetadata session filename currentId
          case result of
            Right bookMetadata ->
              createOrUpdateBookInfo bookInfoId isRead bookMetadata
            Left str ->
              liftIO $ putStrLn $ "error reading EPUB file: " ++ str
          liftIO $ putStrLn $ prefix ++ ": " ++ show filename
        let
          newProcessedSet =
            S.insert currentId processedSet
          newReadSet =
            if S.member parentItemId readSet
            then S.insert currentId readSet
            else readSet
        put (newReadSet, newProcessedSet)
      return ()


getAuth :: IO Auth
getAuth =
  maybe NoAuth auth <$> tryGetAdminAuth
  where
    auth (username, password) =
      BasicAuth username password


tryGetAdminAuth :: IO (Maybe (ByteString, ByteString))
tryGetAdminAuth = runMaybeT $ do
  username <- MaybeT $ lookupEnv "COUCHDB_ADMIN_USERNAME"
  password <- MaybeT $ lookupEnv "COUCHDB_ADMIN_PASSWORD"
  return (pack username, pack password)
