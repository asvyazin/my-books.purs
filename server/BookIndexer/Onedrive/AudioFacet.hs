{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.AudioFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Int (Int64)
import Data.Text (Text)


data AudioFacet =
  AudioFacet
  { audioFacetAlbum :: Maybe Text
  , audioFacetAlbumArtist :: Maybe Text
  , audioFacetArtist :: Maybe Text
  , audioFacetBitrate :: Maybe Int
  , audioFacetComposers :: Maybe Text
  , audioFacetCopyright :: Maybe Text
  , audioFacetDisk :: Maybe Int
  , audioFacetDiskCount :: Maybe Int
  , audioFacetDuration :: Int64
  , audioFacetGenre :: Maybe Text
  , audioFacetHasDrm :: Bool
  , audioFacetIsVariableBitrate :: Bool
  , audioFacetTitle :: Maybe Text
  , audioFacetTrack :: Maybe Int
  , audioFacetTrackCount :: Maybe Int
  , audioFacetYear :: Maybe Int
  } deriving (Show)


instance FromJSON AudioFacet where
  parseJSON (Object o) =
    AudioFacet <$>
    o .:? "album" <*>
    o .:? "albumArtist" <*>
    o .:? "artist" <*>
    o .:? "bitrate" <*>
    o .:? "composers" <*>
    o .:? "copyright" <*>
    o .:? "disk" <*>
    o .:? "diskCount" <*>
    o .: "duration" <*>
    o .:? "genre" <*>
    o .: "hasDrm" <*>
    o .: "isVariableBitrate" <*>
    o .:? "title" <*>
    o .:? "track" <*>
    o .:? "trackCount" <*>
    o .:? "year"
  parseJSON _ =
    error "Invalid AudioFacet JSON"
