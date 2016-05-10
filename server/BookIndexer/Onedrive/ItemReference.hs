{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.Onedrive.ItemReference where


import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Text (Text)


data ItemReference =
  ItemReference
  { itemReferenceDriveId :: Text
  , itemReferenceId_ :: Text
  , itemReferencePath :: Text
  } deriving (Show)


instance FromJSON ItemReference where
  parseJSON (Object o) =
    ItemReference <$> o .: "driveId" <*> o .: "id" <*> o .: "path"
  parseJSON _ =
    error "Invalid ItemReference JSON"
