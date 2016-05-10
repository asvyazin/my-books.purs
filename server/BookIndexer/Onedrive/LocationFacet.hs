{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.LocationFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))


data LocationFacet =
  LocationFacet
  { locationFacetAttitude :: Double
  , locationFacetLatitude :: Double
  , locationFacetLongitude :: Double
  } deriving (Show)


instance FromJSON LocationFacet where
  parseJSON (Object o) =
    LocationFacet <$> o .: "attitude" <*> o .: "latitude" <*> o .: "longitude"
  parseJSON _ =
    error "Invalid LocationFacet JSON"
