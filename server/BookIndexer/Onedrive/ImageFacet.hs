{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.ImageFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))


data ImageFacet =
  ImageFacet
  { imageFacetWidth :: Int
  , imageFacetHeight :: Int
  } deriving (Show)


instance FromJSON ImageFacet where
  parseJSON (Object o) =
    ImageFacet <$> o .: "width" <*> o .: "height"
  parseJSON _ =
    error "Invalid ImageFacet JSON"
