{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.FileFacet where


import BookIndexer.Onedrive.HashesType (HashesType)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Text (Text)


data FileFacet =
  FileFacet
  { fileFacetMimeType :: Text
  , fileFacetHashes :: HashesType
  } deriving (Show)


instance FromJSON FileFacet where
  parseJSON (Object o) =
    FileFacet <$> o .: "mimeType" <*> o .: "hashes"
  parseJSON _ =
    error "Invalid FileFacet JSON"
