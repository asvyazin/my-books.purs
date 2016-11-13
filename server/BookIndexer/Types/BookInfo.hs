{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module BookIndexer.Types.BookInfo (BookInfo(..), id_, rev, read_, token, author, title, epubVersion) where


import Control.Lens (makeLensesWith, camelCaseFields, (^.))
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=), object, withObject)
import Data.Maybe (catMaybes)
import Data.Text (Text)


data BookInfo =
  BookInfo
  { bookInfoId_ :: Text
  , bookInfoRev :: Maybe Text
  , bookInfoRead_ :: Bool
  , bookInfoToken :: Text
  , bookInfoAuthor :: Maybe Text
  , bookInfoTitle :: Maybe Text
  , bookInfoEpubVersion :: Maybe Text
  }


makeLensesWith camelCaseFields ''BookInfo


instance FromJSON BookInfo where
  parseJSON = withObject "Invalid BookInfo JSON" $ \o ->
    BookInfo <$> o .: "_id" <*> o .: "_rev" <*> o .: "read" <*> o .: "token" <*> o .:? "author" <*> o .:? "title" <*> o .:? "epubVersion"


instance ToJSON BookInfo where
  toJSON b =
    object $ [ "_id" .= (b ^. id_)
        , "read" .= (b ^. read_)
        , "type" .= ("book" :: Text)
        , "token" .= (b ^. token) ] ++
    catMaybes [ ("_rev" .=) <$> (b ^. rev)
              , ("author" .=) <$> (b ^. author)
              , ("title" .=) <$> (b ^. title)
              , ("epubVersion" .=) <$> (b ^. epubVersion)
              ]
