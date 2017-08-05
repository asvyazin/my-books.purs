{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.BooksDirectoryInfo where


import Control.Lens (makeLensesWith, camelCaseFields)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Text (Text)


data BooksDirectoryInfo =
  BooksDirectoryInfo
  { booksDirectoryInfoId_ :: Text
  , booksDirectoryInfoRev :: Maybe Text
  , booksDirectoryInfoBooksItemId :: Text
  , booksDirectoryInfoReadItemId :: Text
  } deriving (Show)


makeLensesWith camelCaseFields ''BooksDirectoryInfo


instance FromJSON BooksDirectoryInfo where
  parseJSON (Object o) =
    BooksDirectoryInfo
    <$> o .: "_id"
    <*> o .:? "_rev"
    <*> o .: "booksItemId"
    <*> o .: "readItemId"
  parseJSON _ =
    error "Invalid BooksDirectoryInfo JSON"


booksDirectoryInfoId :: Text
booksDirectoryInfoId =
  "booksDirectoryInfo"


defaultBooksDirectoryInfo :: BooksDirectoryInfo
defaultBooksDirectoryInfo =
  BooksDirectoryInfo booksDirectoryInfoId Nothing "" ""
