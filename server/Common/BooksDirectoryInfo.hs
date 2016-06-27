{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.BooksDirectoryInfo where


import Control.Lens (makeLensesWith, camelCaseFields)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Network.HTTP.Simple (parseRequest, httpJSON, getResponseBody)


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


getBooksDirectoryInfo :: (MonadThrow m, MonadIO m) => Text -> Text -> m BooksDirectoryInfo
getBooksDirectoryInfo couchdbUrl databaseId = do
  req <- parseRequest $ unpack $ couchdbUrl <> "/" <> databaseId <> "/" <> booksDirectoryInfoId
  getResponseBody <$> httpJSON req


booksDirectoryInfoId :: Text
booksDirectoryInfoId =
  "booksDirectoryInfo"
