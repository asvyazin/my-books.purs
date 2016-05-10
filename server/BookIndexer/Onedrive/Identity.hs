{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.Identity where


import BookIndexer.Onedrive.ThumbnailSet (ThumbnailSet)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Text (Text)


data Identity =
  Identity
  { identityId_ :: Text
  , identityDisplayName :: Maybe Text
  , identityThumbnails :: Maybe ThumbnailSet
  } deriving (Show)


instance FromJSON Identity where
  parseJSON (Object o) =
    Identity <$> o .: "id" <*> o .:? "displayName" <*> o .:? "thumbnails"
  parseJSON _ =
    error "Invalid Identity JSON"
