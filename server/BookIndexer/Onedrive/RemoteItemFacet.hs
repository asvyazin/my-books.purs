{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.RemoteItemFacet where


import BookIndexer.Onedrive.FileFacet (FileFacet)
import BookIndexer.Onedrive.FileSystemInfoFacet (FileSystemInfoFacet)
import BookIndexer.Onedrive.FolderFacet (FolderFacet)
import BookIndexer.Onedrive.ItemReference (ItemReference)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Int (Int64)
import Data.Text (Text)


data RemoteItemFacet =
  RemoteItemFacet
  { remoteItemFacetId_ :: Text
  , remoteItemFacetParentReference :: Maybe ItemReference
  , remoteItemFacetFolder :: Maybe FolderFacet
  , remoteItemFacetFile :: Maybe FileFacet
  , remoteItemFacetFileSystemInfo :: Maybe FileSystemInfoFacet
  , remoteItemFacetSize :: Int64
  , remoteItemFacetWebUrl :: Text
  } deriving (Show)


instance FromJSON RemoteItemFacet where
  parseJSON (Object o) =
    RemoteItemFacet <$> o .: "id" <*> o .:? "parentReference" <*> o .:? "folder" <*> o .:? "file" <*> o .:? "fileSystemInfo" <*> o .: "size" <*> o .: "webUrl"
  parseJSON _ =
    error "Invalid RemoteItemFacet JSON"
