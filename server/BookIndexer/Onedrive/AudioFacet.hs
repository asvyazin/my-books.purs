{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.AudioFacet where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Int (Int64)
import Data.Text (Text)


data AudioFacet =
  AudioFacet
  { audioFacetAlbum :: Text
  , audioFacetAlbumArtist :: Text
  , audioFacetArtist :: Text
  , audioFacetBitrate :: Int
  , audioFacetComposers :: Text
  , audioFacetCopyright :: Text
  , audioFacetDisk :: Int
  , audioFacetDiskCount :: Int
  , audioFacetDuration :: Int64
  , audioFacetGenre :: Text
  , audioFacetHasDrm :: Bool
  , audioFacetIsVariableBitrate :: Bool
  , audioFacetTitle :: Text
  , audioFacetTrack :: Int
  , audioFacetTrackCount :: Int
  , audioFacetYear :: Int
  } deriving (Show)


instance FromJSON AudioFacet where
  parseJSON (Object o) =
    AudioFacet <$> o .: "album" <*> o .: "albumArtist" <*> o .: "artist" <*> o .: "bitrate" <*> o .: "composers" <*> o .: "copyright" <*> o .: "disk" <*> o .: "diskCount" <*> o .: "duration" <*> o .: "genre" <*> o .: "hasDrm" <*> o .: "isVariableBitrate" <*> o .: "title" <*> o .: "track" <*> o .: "trackCount" <*> o .: "year"
  parseJSON _ =
    error "Invalid AudioFacet JSON"
