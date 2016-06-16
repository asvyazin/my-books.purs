{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where


import BookIndexer.CouchDB.Changes.Watcher (watchChanges, WatchParams(..), DocumentChange)
import qualified BookIndexer.CouchDB.Changes.Watcher as W (_id)
import BookIndexer.IndexerState (IndexerState(IndexerState), lastSeq, indexerStateId)
-- import Codec.Epub (getPkgPathXmlFromBS, getMetadata)
-- import Codec.Epub.Data.Metadata (Metadata(..), Creator(..), Title(..))
import Common.BooksDirectoryInfo (getBooksDirectoryInfo, booksItemId)
import Common.Database (usersDatabaseName, indexerDatabaseName, userDatabaseName, usersFilter)
import qualified Common.Onedrive as OD (getOnedriveClientSecret)
import Common.OnedriveInfo (getOnedriveInfo, token, refreshToken)
import Common.ServerEnvironmentInfo (getServerEnvironment, couchdbServer, onedriveClientId, appBaseUrl, ServerEnvironmentInfo)
import Common.UserInfo (UserInfo)
import qualified Common.UserInfo as U (_id)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, modifyTVar, TVar)
import Control.Exception.Base (SomeException)
import Control.Lens ((^.), view, set, _Just)
import Control.Monad (void, when, unless)
import Control.Monad.Catch (MonadThrow(throwM), MonadCatch(catch), MonadMask)
-- import Control.Monad.Error.Class (MonadError)
-- import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.State (runStateT)
import Control.Monad.State.Class (MonadState(get, put))
-- import qualified Data.ByteString.Lazy as BL (toStrict)
import Data.Conduit (($$), (=$=))
import qualified Data.Conduit.Combinators as DC (last, mapM_, concatMapM)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Set as S (Set, member, insert, empty, singleton)
import Data.Text (Text, unpack {-, takeEnd, intercalate, pack-})
import Network.HTTP.Client.Conduit (HttpException(StatusCodeException))
import Network.HTTP.Simple (setRequestMethod, setRequestBodyJSON, parseRequest, httpJSON, getResponseBody, httpLBS)
import Network.HTTP.Types.Status (notFound404, unauthorized401)
import Onedrive.Auth (requestRefreshToken)
import Onedrive.FolderChangesReader (newFolderChangesReader, enumerateChanges, getCurrentEnumerationToken)
-- import Onedrive.Items (item)
import qualified Onedrive.Types.ItemReference as IR (id_)
import Onedrive.Types.OnedriveItem (name, id_, parentReference, OnedriveItem)
import Onedrive.Types.OauthTokenRequest (OauthTokenRequest(OauthTokenRequest))
import Onedrive.Types.OauthTokenResponse (accessToken)


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


processWatchItem :: (MonadIO m, MonadReader ServerEnvironmentInfo m, MonadThrow m) => UserSynchronizers -> DocumentChange -> m (Maybe Int64)
processWatchItem userSynchronizers documentChange =
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


synchronizeUserLoop :: (MonadCatch m, MonadIO m, MonadReader ServerEnvironmentInfo m) => UserInfo -> m ()
synchronizeUserLoop userInfo = do
  couchdbUrl <- view couchdbServer
  let
    userDatabaseId =
      userDatabaseName $ userInfo ^. U._id
  onedriveInfo <- getOnedriveInfo couchdbUrl userDatabaseId
  booksDirectoryInfo <- getBooksDirectoryInfo couchdbUrl userDatabaseId
  let
    tok =
      onedriveInfo ^. token
    booksFolder =
      booksDirectoryInfo ^. booksItemId
  liftIO $ putStrLn $ "Books itemId: " ++ show booksFolder
  onedriveReader <- newFolderChangesReader tok booksFolder Nothing
  void (runStateT (reauthorizeLoop readChanges doRefreshToken (onedriveReader, tok)) (S.empty, S.singleton booksFolder)) `catch` logError
  where
    readChanges (reader, tok) =
      enumerateChanges reader $$ DC.mapM_ (processItem tok)

    doRefreshToken (oldReader, _) = do
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
          OauthTokenRequest (env ^. onedriveClientId) ((env ^. appBaseUrl) <> "/onedrive-redirect") secret
      resp <- requestRefreshToken req tok
      -- TODO update onedriveInfo in DB
      currentEnumerationToken <- getCurrentEnumerationToken oldReader
      booksDirectoryInfo <- getBooksDirectoryInfo couchdbUrl userDatabaseId
      let
        newAccessToken =
          resp ^. accessToken
      newReader <- newFolderChangesReader newAccessToken (booksDirectoryInfo ^. booksItemId) currentEnumerationToken
      return (newReader, newAccessToken)

    logError :: (MonadThrow m, MonadIO m) => SomeException -> m a
    logError e = do
      liftIO $ print e
      throwM e

    processItem :: (MonadThrow m, MonadIO m, MonadState (S.Set Text, S.Set Text) m) => Text -> OnedriveItem -> m ()
    processItem _ i = do
      (readSet, processedSet) <- get 
      unless (S.member (i ^. id_) processedSet) $ do
        let
          filename = i ^. name
          parentItemId = i ^. parentReference . _Just . IR.id_
        unless (S.member parentItemId processedSet) $
          liftIO $ putStrLn $ "Parent itemId not found: " ++ show parentItemId
        when (S.member parentItemId readSet) $
          liftIO $ putStrLn $ "Read: " ++ show filename
        let
          newProcessedSet = S.insert (i ^. id_) processedSet
          newReadSet =
            if filename == "read" || S.member parentItemId readSet
            then S.insert (i ^. id_) readSet
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
