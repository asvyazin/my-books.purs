{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.FolderFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Int (Int64)


data FolderFacet =
  FolderFacet
  { folderFacetChildCount :: Int64
  } deriving (Show)


instance FromJSON FolderFacet where
  parseJSON (Object o) =
    FolderFacet <$> o .: "childCount"
  parseJSON _ =
    error "Invalid FolderFacet JSON"
