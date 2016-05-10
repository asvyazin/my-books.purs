{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.ThumbnailSet where


import BookIndexer.Onedrive.Thumbnail (Thumbnail)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Text (Text)


data ThumbnailSet =
  ThumbnailSet
  { thumbnailSetId_ :: Text
  , thumbnailSmall :: Maybe Thumbnail
  , thumbnailMedium :: Maybe Thumbnail
  , thumbnailLarge :: Maybe Thumbnail
  , thumbnailSource :: Maybe Thumbnail
  } deriving (Show)


instance FromJSON ThumbnailSet where
  parseJSON (Object o) =
    ThumbnailSet <$> o .: "id" <*> o.:? "small" <*> o .:? "medium" <*> o .:? "large" <*> o .:? "source"
  parseJSON _ =
    error "Invalid ThumbnailSet JSON"
