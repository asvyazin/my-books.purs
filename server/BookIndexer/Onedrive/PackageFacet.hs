{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.PackageFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Text (Text)


data PackageFacet =
  PackageFacet
  { packageFacetType :: Text
  } deriving (Show)


instance FromJSON PackageFacet where
  parseJSON (Object o) =
    PackageFacet <$> o .: "type"
  parseJSON _ =
    error "Invalid PackageFacet JSON"
