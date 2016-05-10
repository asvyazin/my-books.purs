{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module BookIndexer.Onedrive.FolderChangesReader where


import Blaze.ByteString.Builder (toByteString)
import BookIndexer.Onedrive.OnedriveItem (OnedriveItem)
import Common.JSONHelper (jsonParser)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, writeTVar, newTVar, readTVar)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.ByteString.Char8 (unpack)
import Data.Conduit (Source, ($$), (=$=))
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Conduit.Combinators as DC (repeatM, concatMap)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client.Conduit (withResponse, responseBody, HasHttpManager)
import Network.HTTP.Simple (parseRequest, setRequestHeaders)
import Network.HTTP.Types.Header (hAuthorization)
import Network.HTTP.Types.URI (encodePath)


data FolderChangesReader =
  FolderChangesReader
  { folderChangesReaderAccessToken :: Text
  , folderChangesReaderItemId :: Text
  , folderChangesReaderCurrentEnumerationToken :: TVar (Maybe Text)
  }


newFolderChangesReader :: (MonadIO m) => Text -> Text -> Maybe Text -> m FolderChangesReader
newFolderChangesReader accessToken itemId enumerationToken = do
  currentToken <- liftIO $ atomically $ newTVar enumerationToken
  return $ FolderChangesReader accessToken itemId currentToken


getCurrentEnumerationToken :: (MonadIO m) => FolderChangesReader -> m (Maybe Text)
getCurrentEnumerationToken reader =
  liftIO $ atomically $ readTVar $ folderChangesReaderCurrentEnumerationToken reader


enumerateChanges :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadReader env m, HasHttpManager env) => FolderChangesReader -> Source m OnedriveItem
enumerateChanges changesReader =
  enumerateChangesBatches changesReader =$= DC.concatMap id


enumerateChangesBatches :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadReader env m, HasHttpManager env) => FolderChangesReader -> Source m [OnedriveItem]
enumerateChangesBatches (FolderChangesReader accessToken itemId currentToken) =
  DC.repeatM getChangesBatch'
  where
    getChangesBatch' = do
      enumerationToken <- liftIO $ atomically $ readTVar currentToken
      batch <- getChangesBatch accessToken itemId enumerationToken
      liftIO $ atomically $ writeTVar currentToken $ Just $ folderChangesBatchToken batch
      return $ folderChangesBatchValue batch


getChangesBatch :: (MonadThrow m, MonadBaseControl IO m, MonadReader env m, HasHttpManager env, MonadIO m) => Text -> Text -> Maybe Text -> m FolderChangesBatch
getChangesBatch accessToken itemId enumerationToken = do
  let
    tokenParam tok =
      ("token", Just (encodeUtf8 tok))
    qsParams =
      maybeToList $ tokenParam <$> enumerationToken
    path =
      toByteString $ encodePath ["drive", "items", itemId, "view.delta"] qsParams
  initReq <- parseRequest $ unpack $ "https://api.onedrive.com/v1.0" <> path
  let
    req = setRequestHeaders [(hAuthorization, encodeUtf8 ("Bearer " <> accessToken))] initReq
  withResponse req $ \resp ->
    responseBody resp $$ sinkParser jsonParser


data FolderChangesBatch =
  FolderChangesBatch
  { folderChangesBatchValue :: [OnedriveItem]
  , folderChangesBatchNextLink :: Maybe Text
  , folderChangesBatchDeltaLink :: Maybe Text
  , folderChangesBatchToken :: Text
  }


instance FromJSON FolderChangesBatch where
  parseJSON (Object o) =
    FolderChangesBatch <$> o .: "value" <*> o .: "@odata.nextLink" <*> o .: "@odata.deltaLink" <*> o .: "@delta.token"
  parseJSON _ =
    error "Invalid FolderChangesBatch JSON"
