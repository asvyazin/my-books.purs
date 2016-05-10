{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.PhotoFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Int (Int64)
import Data.Text (Text)


data PhotoFacet =
  PhotoFacet
  { photoFacetTakenDateTime :: Text
  , photoFacetCameraMake :: Text
  , photoFacetCameraModel :: Text
  , photoFacetFNumber :: Double
  , photoFacetExposureDenominator :: Double
  , photoFacetExpusureNumerator :: Double
  , photoFacetFocalLength :: Double
  , photoFacetIso :: Int64
  } deriving (Show)


instance FromJSON PhotoFacet where
  parseJSON (Object o) =
    PhotoFacet <$> o .: "takenDateTime" <*> o .: "cameraMake" <*> o .: "cameraModel" <*> o .: "fNumber" <*> o .: "exposureDenominator" <*> o .: "exposureNumerator" <*> o .: "focalLength" <*> o .: "iso"
  parseJSON _ =
    error "Invalid PhotoFacet JSON"
