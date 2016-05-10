{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.VideoFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))


data VideoFacet =
  VideoFacet
  { videoFacetBitrate :: Int
  , videoFacetDuration :: Int
  , videoFacetHeight :: Int
  , videoFacetWidth :: Int
  } deriving (Show)


instance FromJSON VideoFacet where
  parseJSON (Object o) =
    VideoFacet <$> o .: "bitrate" <*> o .: "duration" <*> o .: "height" <*> o .: "width"
  parseJSON _ =
    error "Invalid VideoFacet JSON"
