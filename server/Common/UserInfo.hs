{-# LANGUAGE OverloadedStrings #-}
module Common.UserInfo where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))


data UserInfo =
  UserInfo
  { __id :: String
  , __rev :: Maybe String
  , _displayName :: String
  } deriving (Show)


instance FromJSON UserInfo where
  parseJSON (Object v) =
    UserInfo <$> v .: "_id" <*> v .:? "_rev" <*> v .: "displayName"
  parseJSON _ =
    error "Invalid UserInfo JSON"
