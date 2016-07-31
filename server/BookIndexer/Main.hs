{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where


import CouchDB.Changes.Watcher (watchChanges, WatchParams(..), DocumentChange)
import qualified CouchDB.Changes.Watcher as W (_id)
import CouchDB.Requests (getObject, putObject)
import BookIndexer.IndexerState (IndexerState(IndexerState), lastSeq, indexerStateId)
import BookIndexer.Types.BookInfo (BookInfo(BookInfo), read_)
import qualified BookIndexer.Types.BookInfo as BI (token)
import CouchDB.Types.Seq (Seq)
-- import Codec.Epub (getPkgPathXmlFromBS, getMetadata)
-- import Codec.Epub.Data.Metadata (Metadata(..), Creator(..), Title(..))
import Common.BooksDirectoryInfo (BooksDirectoryInfo, getBooksDirectoryInfo, booksItemId, readItemId)
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
-- import Control.Monad.Error.Class (MonadError)
-- import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.State (runStateT)
import Control.Monad.State.Class (MonadState(get, put))
import Control.Monad.Trans.Maybe (runMaybeT)
-- import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.Conduit (($$), (=$=))
import qualified Data.Conduit.Combinators as DC (last, mapM_, concatMapM)
import Data.Maybe (fromJust, isNothing, maybe, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as S (Set, member, insert, singleton)
import Data.Text (Text, {-, takeEnd, intercalate, pack-})
import Onedrive.Auth (requestRefreshToken)
import Onedrive.FolderChangesReader (newFolderChangesReader, enumerateChanges)
-- import Onedrive.Items (item)
import Onedrive.Session (newSessionWithRenewableToken)
import qualified Onedrive.Types.ItemReference as IR (id_)
import Onedrive.Types.OnedriveItem (name, id_, parentReference, OnedriveItem, folder)
import Onedrive.Types.OauthTokenRequest (OauthTokenRequest(OauthTokenRequest))
import Onedrive.Types.OauthTokenResponse (accessToken)


type UserSynchronizers =
  TVar [Async ()]


main :: IO ()
main = do
  serverEnv <- getServerEnvironment
  let couchdb = serverEnv ^. couchdbServer
  userSynchronizers <- atomically $ newTVar []
  state <- getObject couchdb indexerDatabaseName indexerStateId
  let
    ls = view lastSeq <$> state
  maybeNewLastSeq <- runReaderT (watchNewUsersLoop userSynchronizers ls) serverEnv
  case maybeNewLastSeq of
    Nothing ->
      return ()
    Just newLastSeq ->
      putObject couchdb indexerDatabaseName indexerStateId $ newIndexerState state newLastSeq


newIndexerState :: Maybe IndexerState -> Seq -> IndexerState
newIndexerState current ls =
  maybe (IndexerState Nothing ls) (set lastSeq ls) current


watchNewUsersLoop :: (MonadMask  m
                     , MonadReader ServerEnvironmentInfo m
                     , MonadIO m) => UserSynchronizers -> Maybe Seq -> m (Maybe Seq)
watchNewUsersLoop userSynchronizers ls = do
  env <- ask
  let
    watchParams =
      WatchParams
      { _baseUrl = env ^. couchdbServer
      , _database = usersDatabaseName
      , _since = ls
      , _filter = Just usersFilter
      }
  watchChanges watchParams (DC.concatMapM (processWatchItem userSynchronizers) =$= DC.last) -- TODO: catch exceptions


processWatchItem :: (MonadIO m
                    , MonadReader ServerEnvironmentInfo m
                    , MonadThrow m) => UserSynchronizers -> DocumentChange -> m (Maybe Seq)
processWatchItem userSynchronizers documentChange = runMaybeT $ do
  ui <- getUserInfo documentChange >>= hoistMaybe
  processUser userSynchronizers ui
  hoistMaybe Nothing


getUserInfo :: (MonadIO m, MonadThrow m, MonadReader ServerEnvironmentInfo m) => DocumentChange -> m (Maybe UserInfo)
getUserInfo documentChange = do
  env <- ask
  let
    couchdb = env ^. couchdbServer
    docId = documentChange ^. W._id
  getObject couchdb usersDatabaseName docId


processUser :: (MonadIO m, MonadReader ServerEnvironmentInfo m) => UserSynchronizers -> UserInfo -> m ()
processUser userSynchronizers userInfo = do
  env <- ask
  synchronizer <- liftIO $ async $ runReaderT (synchronizeUserLoop userInfo) env
  liftIO $ atomically $ modifyTVar userSynchronizers (synchronizer :)
  return ()


synchronizeUserLoop :: (MonadCatch m, MonadIO m, MonadReader ServerEnvironmentInfo m) => UserInfo -> m ()
synchronizeUserLoop userInfo = do
  serverEnv <- ask
  let
    couchdb =
      serverEnv ^. couchdbServer
    userDatabaseId =
      userDatabaseName $ userInfo ^. U._id
  onedriveInfo <- fromMaybe defaultOnedriveInfo <$> getObject couchdb userDatabaseId onedriveInfoId
  booksDirectoryInfo <- getBooksDirectoryInfo couchdb userDatabaseId
  let
    tok =
      onedriveInfo ^. token
    booksFolder =
      booksDirectoryInfo ^. booksItemId
    readFolder =
      booksDirectoryInfo ^. readItemId
  liftIO $ putStrLn $ "Books itemId: " ++ show booksFolder
  session <- liftIO $ newSessionWithRenewableToken tok (runReaderT renewToken serverEnv)
  onedriveReader <- newFolderChangesReader session booksFolder Nothing
  let
    loop =
      enumerateChanges onedriveReader $$ DC.mapM_ (processItem couchdb booksDirectoryInfo)
    startState =
      (S.singleton readFolder, S.singleton booksFolder)
  void (runStateT loop startState) `catch` logError
  where
    renewToken = do
      secret <- liftIO OD.getOnedriveClientSecret
      env <- ask
      let
        couchdbUrl =
          env ^. couchdbServer
        userDatabaseId =
          userDatabaseName $ userInfo ^. U._id
      onedriveInfo <- fromMaybe defaultOnedriveInfo <$> getObject couchdbUrl userDatabaseId onedriveInfoId
      let
        tok =
          fromJust (onedriveInfo ^. refreshToken)
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
                   , MonadState (S.Set Text, S.Set Text) m) => Text -> BooksDirectoryInfo -> OnedriveItem -> m ()
    processItem couchdb booksDirectoryInfo i = do
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

        createOrUpdateBookInfo itemId isRead = do
          let
            defaultBook =
              BookInfo bookInfoId Nothing isRead bookInfoToken
            updateBook =
              set BI.token bookInfoToken . set read_ isRead
          newBook <- maybe defaultBook updateBook <$> getObject couchdb userDatabaseId itemId
          putObject couchdb userDatabaseId itemId newBook

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
          createOrUpdateBookInfo bookInfoId isRead
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


    {-
    processItem1 :: (MonadThrow m, MonadIO m) => Text -> OnedriveItem -> m ()
    processItem1 tok i =
      when (isInterestingItem i) $ do
        result <- runExceptT $ processEpubItem tok i
        case result of
          Right _ ->
            return ()
          Left strError ->
            liftIO $ putStrLn $ "Error reading EPUB " ++ show (i ^. name) ++ ": " ++ strError

    isInterestingItem i =
      let
        filename = i ^. name
        ext = takeEnd 5 filename
      in
        ext == ".epub"

    processEpubItem :: (MonadThrow m, MonadIO m, MonadError String m) => Text -> OnedriveItem -> m ()
    processEpubItem tok i = do
      let filename = i ^. name
      c <- content tok $ i ^. id_
      (_, xmlString) <- getPkgPathXmlFromBS $ BL.toStrict c
      metadata <- getMetadata xmlString
      liftIO $ print filename
      liftIO $ putStrLn $ show (getAuthor metadata) ++ " - " ++ show (getTitle metadata)

    getAuthor :: Metadata -> Text
    getAuthor meta =
      let
        author =
          intercalate ", " $ map (pack . creatorText) $ metaCreators meta
      in
        "(" <> author <> ")"

    getTitle :: Metadata -> Text
    getTitle meta =
      let
        title =
          intercalate "; " $ map (pack . titleText) $ metaTitles meta
      in
        "[" <> title <> "]"
   -}
