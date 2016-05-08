{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.OnedriveInfo (OnedriveInfo, _id, _rev, token, onedriveInfoId) where


import Control.Lens (makeLenses)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Text (Text)


data OnedriveInfo =
  OnedriveInfo
  { __id :: Text
  , __rev :: Maybe Text
  , _token :: Text
  } deriving (Show)


makeLenses ''OnedriveInfo


instance FromJSON OnedriveInfo where
  parseJSON (Object v) =
    OnedriveInfo <$> v .: "_id" <*> v .:? "_rev" <*> v .: "token"
  parseJSON _ =
    error "Invalid OnedriveInfo JSON"


onedriveInfoId :: Text
onedriveInfoId =
  "onedriveInfo"
