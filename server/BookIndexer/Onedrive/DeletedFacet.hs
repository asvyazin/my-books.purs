{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.DeletedFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Text (Text)


data DeletedFacet =
  DeletedFacet
  { state :: Text
  } deriving (Show)


instance FromJSON DeletedFacet where
  parseJSON (Object o) =
    DeletedFacet <$> o .: "state"
  parseJSON _ =
    error "Invalid DeletedFacet JSON"
