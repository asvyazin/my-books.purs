{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.HashesType where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:?))
import Data.Text (Text)


data HashesType =
  HashesType
  { hashesTypeCrc32Hash :: Maybe Text
  , hashesTypeSha1Hash :: Maybe Text
  } deriving (Show)


instance FromJSON HashesType where
  parseJSON (Object o) =
    HashesType <$> o .:? "crc32Hash" <*> o .:? "sha1Hash"
  parseJSON _ =
    error "Invalid HashesType JSON"
