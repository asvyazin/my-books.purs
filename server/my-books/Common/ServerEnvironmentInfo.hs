{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.ServerEnvironmentInfo where


import Control.Lens (makeLenses)
import Data.Aeson (ToJSON(toJSON), object, (.=))
import qualified Data.Text as T
import System.Environment (lookupEnv)


data ServerEnvironmentInfo =
  ServerEnvironmentInfo
  { _appBaseUrl :: T.Text
  , _onedriveClientId :: T.Text
  , _couchdbServer :: T.Text
  , _userCouchdbServer :: T.Text
  }


makeLenses ''ServerEnvironmentInfo


instance ToJSON ServerEnvironmentInfo where
  toJSON (ServerEnvironmentInfo baseUrl clientId couchdb userCouchdb) =
    object [ "baseUrl" .= baseUrl
           , "onedriveClientId" .= clientId
           , "couchdbServer" .= couchdb
           , "userCouchdbServer" .= userCouchdb
           ]


getServerEnvironment :: IO ServerEnvironmentInfo
getServerEnvironment =
  ServerEnvironmentInfo <$> getAppBaseUrl <*> getOnedriveClientId <*> getCouchdbServer <*> getUserCouchdbServer


getOnedriveClientId :: IO T.Text
getOnedriveClientId =
  maybe "000000004816D42C" T.pack <$> lookupEnv "ONEDRIVE_CLIENT_ID"


getAppBaseUrl :: IO T.Text
getAppBaseUrl =
  maybe "http://localhost:8000" T.pack <$> lookupEnv "APP_BASE_URL"


getCouchdbServer :: IO T.Text
getCouchdbServer =
  maybe "http://localhost:5984" T.pack <$> lookupEnv "COUCHDB_SERVER"


getUserCouchdbServer :: IO T.Text
getUserCouchdbServer =
  maybe "http://localhost:8001" T.pack <$> lookupEnv "COUCHDB_USER_SERVER"
