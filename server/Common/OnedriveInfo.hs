{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.OnedriveInfo where


import Common.JSONHelper (jsonParser)
import Control.Lens (makeLenses, (^.))
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?), ToJSON(toJSON), object, (.=))
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Network.HTTP.Client.Conduit (HasHttpManager, withResponse, responseBody, httpNoBody)
import Network.HTTP.Simple (parseRequest, setRequestMethod, setRequestBodyJSON)


data OnedriveInfo =
  OnedriveInfo
  { __id :: Text
  , __rev :: Maybe Text
  , _token :: Text
  , _refreshToken :: Maybe Text
  } deriving (Show)


makeLenses ''OnedriveInfo


instance FromJSON OnedriveInfo where
  parseJSON (Object v) =
    OnedriveInfo <$> v .: "_id" <*> v .:? "_rev" <*> v .: "token" <*> v .:? "refresh_token"
  parseJSON _ =
    error "Invalid OnedriveInfo JSON"


instance ToJSON OnedriveInfo where
  toJSON o =
    let
      state =
        [ Just ("_id" .= (o ^. _id))
        , Just ("token" .= (o ^. token))
        , Just ("refresh_token" .= (o ^. refreshToken))
        , formatRev <$> (o ^. _rev)
        ]
      formatRev str =
        "_rev" .= str
    in
      object $ catMaybes state


onedriveInfoId :: Text
onedriveInfoId =
  "onedriveInfo"


onedriveInfoUrl :: Text -> Text -> String
onedriveInfoUrl couchdbUrl databaseId =
  unpack $ couchdbUrl <> "/" <> databaseId <> "/" <> onedriveInfoId


getOnedriveInfo :: (MonadReader env m, HasHttpManager env, MonadThrow m, MonadBaseControl IO m, MonadIO m) => Text -> Text -> m OnedriveInfo
getOnedriveInfo couchdbUrl databaseId = do
  req <- parseRequest $ onedriveInfoUrl couchdbUrl databaseId
  withResponse req $ \resp ->
    responseBody resp $$ sinkParser jsonParser


setOnedriveInfo :: (MonadReader env m, HasHttpManager env, MonadThrow m, MonadBaseControl IO m, MonadIO m) => Text -> Text -> OnedriveInfo -> m ()
setOnedriveInfo couchdbUrl databaseId onedriveInfo = do
  initReq <- parseRequest $ onedriveInfoUrl couchdbUrl databaseId
  let
    req =
      setRequestMethod "PUT" $ setRequestBodyJSON onedriveInfo initReq
  void $ httpNoBody req
