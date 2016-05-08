{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.BooksDirectoryInfo where


import Common.JSONHelper (jsonParser)
import Control.Lens (makeLensesWith, camelCaseFields)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:), (.:?))
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Network.HTTP.Client.Conduit (HasHttpManager, withResponse, responseBody)
import Network.HTTP.Simple (parseRequest)


data BooksDirectoryInfo =
  BooksDirectoryInfo
  { booksDirectoryInfoId_ :: Text
  , booksDirectoryInfoRev :: Maybe Text
  , booksDirectoryInfoBooksItemId :: Text
  } deriving (Show)


makeLensesWith camelCaseFields ''BooksDirectoryInfo


instance FromJSON BooksDirectoryInfo where
  parseJSON (Object o) =
    BooksDirectoryInfo <$> o .: "_id" <*> o .:? "_rev" <*> o .: "booksItemId"
  parseJSON _ =
    error "Invalid BooksDirectoryInfo JSON"


getBooksDirectoryInfo :: (MonadReader env m, HasHttpManager env, MonadThrow m, MonadIO m, MonadBaseControl IO m) => Text -> Text -> m BooksDirectoryInfo
getBooksDirectoryInfo couchdbUrl databaseId = do
  req <- parseRequest $ unpack $ couchdbUrl <> "/" <> databaseId <> "/" <> booksDirectoryInfoId
  withResponse req $ \resp ->
    responseBody resp $$ sinkParser jsonParser


booksDirectoryInfoId :: Text
booksDirectoryInfoId =
  "booksDirectoryInfo"
