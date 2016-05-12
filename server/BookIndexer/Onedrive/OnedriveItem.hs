{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.OnedriveItem where


import BookIndexer.Onedrive.AudioFacet (AudioFacet)
import BookIndexer.Onedrive.DeletedFacet (DeletedFacet)
import BookIndexer.Onedrive.FileFacet (FileFacet)
import BookIndexer.Onedrive.FileSystemInfoFacet (FileSystemInfoFacet)
import BookIndexer.Onedrive.FolderFacet (FolderFacet)
import BookIndexer.Onedrive.IdentitySet (IdentitySet)
import BookIndexer.Onedrive.ImageFacet (ImageFacet)
import BookIndexer.Onedrive.ItemReference (ItemReference)
import BookIndexer.Onedrive.LocationFacet (LocationFacet)
import BookIndexer.Onedrive.PackageFacet (PackageFacet)
import BookIndexer.Onedrive.PhotoFacet (PhotoFacet)
import BookIndexer.Onedrive.RemoteItemFacet (RemoteItemFacet)
import BookIndexer.Onedrive.SearchResultFacet (SearchResultFacet)
import BookIndexer.Onedrive.SharedFacet (SharedFacet)
import BookIndexer.Onedrive.SharepointIdsFacet (SharepointIdsFacet)
import BookIndexer.Onedrive.SpecialFolderFacet (SpecialFolderFacet)
import BookIndexer.Onedrive.VideoFacet (VideoFacet)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Int (Int64)
import Data.Text (Text)


data OnedriveItem =
  OnedriveItem
  { onedriveItemId_ :: Text
  , onedriveItemAudio :: Maybe AudioFacet
  , onedriveItemCreatedBy :: IdentitySet
  , onedriveItemCreatedDateTime :: Text
  , onedriveItemCTag :: Text
  , onedriveItemDeleted :: Maybe DeletedFacet
  , onedriveItemDescription :: Maybe Text
  , onedriveItemETag :: Text
  , onedriveItemFile :: Maybe FileFacet
  , onedriveItemFileSystemInfo :: FileSystemInfoFacet
  , onedriveItemFolder :: Maybe FolderFacet
  , onedriveItemImage :: Maybe ImageFacet
  , onedriveItemLastModifiedBy :: IdentitySet
  , onedriveItemLastModifiedDateTime :: Text
  , onedriveItemLocation :: Maybe LocationFacet
  , onedriveItemName :: Text
  , onedriveItemPackage :: Maybe PackageFacet
  , onedriveItemParentReference :: Maybe ItemReference
  , onedriveItemPhoto :: Maybe PhotoFacet
  , onedriveItemRemoteItem :: Maybe RemoteItemFacet
  , onedriveItemSearchResult :: Maybe SearchResultFacet
  , onedriveItemShared :: Maybe SharedFacet
  , onedriveItemSharepointIds :: Maybe SharepointIdsFacet
  , onedriveItemSize :: Int64
  , onedriveItemSpecialFolder :: Maybe SpecialFolderFacet
  , onedriveItemVideo :: Maybe VideoFacet
  , onedriveItemWebDavUrl :: Maybe Text
  , onedriveItemWebUrl :: Text
  } deriving (Show)


instance FromJSON OnedriveItem where
  parseJSON (Object o) =
    OnedriveItem <$>
    o .: "id" <*>
    o .:? "audio" <*>
    o .: "createdBy" <*>
    o .: "createdDateTime" <*>
    o .: "cTag" <*>
    o .:? "deleted" <*>
    o .:? "description" <*>
    o .: "eTag" <*>
    o .:? "file" <*>
    o .: "fileSystemInfo" <*>
    o .:? "folder" <*>
    o .:? "image" <*>
    o .: "lastModifiedBy" <*>
    o .: "lastModifiedDateTime" <*>
    o .:? "location" <*>
    o .: "name" <*>
    o .:? "package" <*>
    o .:? "parentReference" <*>
    o .:? "photo" <*>
    o .:? "remoteItem" <*>
    o .:? "searchResult" <*>
    o .:? "shared" <*>
    o .:? "sharepointIds" <*>
    o .: "size" <*>
    o .:? "specialFolder" <*>
    o .:? "video" <*>
    o .:? "webDavUrl" <*>
    o .: "webUrl"
  parseJSON _ =
    error "Invalid OnedriveItem JSON"
