{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.SpecialFolderFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Text (Text)


data SpecialFolderFacet =
  SpecialFolderFacet
  { specialFolderFacetName :: Text
  } deriving (Show)


instance FromJSON SpecialFolderFacet where
  parseJSON (Object o) =
    SpecialFolderFacet <$> o .: "name"
  parseJSON _ =
    error "Invalid SpecialFolderFacet JSON"
