{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.Thumbnail where


import Data.Aeson (FromJSON(parseJSON), (.:), Value(Object))
import Data.Text (Text)


data Thumbnail =
  Thumbnail
  { thumbnailWidth :: Int
  , thumbnailHeight :: Int
  , thumbnailUrl :: Text
  } deriving (Show)


instance FromJSON Thumbnail where
  parseJSON (Object o) =
    Thumbnail <$> o .: "width" <*> o .: "height" <*> o .: "url"
  parseJSON _ =
    error "Invalid Thumbnail JSON"
