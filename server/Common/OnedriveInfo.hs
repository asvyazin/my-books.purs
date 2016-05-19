{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.OnedriveInfo where


import Control.Lens (makeLenses, (^.))
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?), ToJSON(toJSON), object, (.=))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Network.HTTP.Simple (parseRequest, setRequestMethod, setRequestBodyJSON, httpJSON, getResponseBody, httpLBS)


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


getOnedriveInfo :: (MonadThrow m, MonadIO m) => Text -> Text -> m OnedriveInfo
getOnedriveInfo couchdbUrl databaseId = do
  req <- parseRequest $ onedriveInfoUrl couchdbUrl databaseId
  getResponseBody <$> httpJSON req


setOnedriveInfo :: (MonadThrow m, MonadIO m) => Text -> Text -> OnedriveInfo -> m ()
setOnedriveInfo couchdbUrl databaseId onedriveInfo = do
  initReq <- parseRequest $ onedriveInfoUrl couchdbUrl databaseId
  let
    req =
      setRequestMethod "PUT" $ setRequestBodyJSON onedriveInfo initReq
  void $ httpLBS req
