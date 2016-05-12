{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.PhotoFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:?))
import Data.Int (Int64)
import Data.Text (Text)


data PhotoFacet =
  PhotoFacet
  { photoFacetTakenDateTime :: Maybe Text
  , photoFacetCameraMake :: Maybe Text
  , photoFacetCameraModel :: Maybe Text
  , photoFacetFNumber :: Maybe Double
  , photoFacetExposureDenominator :: Maybe Double
  , photoFacetExpusureNumerator :: Maybe Double
  , photoFacetFocalLength :: Maybe Double
  , photoFacetIso :: Maybe Int64
  } deriving (Show)


instance FromJSON PhotoFacet where
  parseJSON (Object o) =
    PhotoFacet <$> o .:? "takenDateTime" <*> o .:? "cameraMake" <*> o .:? "cameraModel" <*> o .:? "fNumber" <*> o .:? "exposureDenominator" <*> o .:? "exposureNumerator" <*> o .:? "focalLength" <*> o .:? "iso"
  parseJSON _ =
    error "Invalid PhotoFacet JSON"
