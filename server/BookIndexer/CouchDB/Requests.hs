{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.CouchDB.Requests where


import Common.ServerEnvironmentInfo (ServerEnvironmentInfo, couchdbServer)
import Control.Lens (view)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Network.HTTP.Client (responseCookieJar)
import Network.HTTP.Simple (parseRequest, httpJSONEither, getResponseStatus, getResponseBody, getResponseHeaders, HttpException(StatusCodeException), setRequestMethod, setRequestBodyJSON, httpLBS)
import Network.HTTP.Types.Status (ok200, notFound404)


getObjectUrl :: MonadReader ServerEnvironmentInfo m => Text -> Text -> m String
getObjectUrl databaseId objectId = do
  couchdbUrl <- view couchdbServer
  return $ unpack $ couchdbUrl <> "/" <> databaseId <> "/" <> objectId


getObject :: (MonadThrow m, MonadIO m, MonadReader ServerEnvironmentInfo m, FromJSON a) => Text -> Text -> m (Maybe a)
getObject databaseName objectId = do
  req <- parseRequest =<< getObjectUrl databaseName objectId
  resp <- httpJSONEither req
  let responseStatus = getResponseStatus resp
  if responseStatus == ok200
    then
    case getResponseBody resp of
      Left e -> throwM e
      Right res -> return $ Just res
    else
    if responseStatus == notFound404
    then return Nothing
    else throwM $ StatusCodeException responseStatus (getResponseHeaders resp) (responseCookieJar resp)


putObject :: (MonadThrow m, MonadIO m, MonadReader ServerEnvironmentInfo m, ToJSON a) => Text -> Text -> a -> m ()
putObject databaseName objectId obj = do
  initReq <- parseRequest =<< getObjectUrl databaseName objectId
  let
    req =
      setRequestMethod "PUT" $ setRequestBodyJSON obj initReq
  void $ httpLBS req
