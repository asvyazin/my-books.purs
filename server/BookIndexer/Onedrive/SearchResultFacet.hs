{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.SearchResultFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Text (Text)


data SearchResultFacet =
  SearchResultFacet
  { searchResultFacetOnClickTelemetryUrl :: Text
  } deriving (Show)


instance FromJSON SearchResultFacet where
  parseJSON (Object o) =
    SearchResultFacet <$> o .: "onClickTelemetryUrl"
  parseJSON _ =
    error "Invalid SearchResultFacet JSON"
