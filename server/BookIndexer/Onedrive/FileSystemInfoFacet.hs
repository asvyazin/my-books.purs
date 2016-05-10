{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.FileSystemInfoFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Text (Text)


data FileSystemInfoFacet =
  FileSystemInfoFacet
  { fileSystemInfoFacetCreatedDateTime :: Text
  , fileSystemInfoFacetLastModifiedDateTime :: Text
  } deriving (Show)


instance FromJSON FileSystemInfoFacet where
  parseJSON (Object o) =
    FileSystemInfoFacet <$> o .: "createdDateTime" <*> o .: "lastModifiedDateTime"
  parseJSON _ =
    error "Invalid FileSystemInfoFacet JSON"
