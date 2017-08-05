{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.UserInfo where


import Control.Lens (makeLenses)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Text (Text)


data UserInfo =
  UserInfo
  { __id  :: Text
  , __rev :: Maybe Text
  , _displayName :: Text
  } deriving (Show)


makeLenses ''UserInfo


instance FromJSON UserInfo where
  parseJSON (Object v) =
    UserInfo <$> v .: "_id" <*> v .:? "_rev" <*> v .: "displayName"
  parseJSON _ =
    error "Invalid UserInfo JSON"
