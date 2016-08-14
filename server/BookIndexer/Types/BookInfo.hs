{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module BookIndexer.Types.BookInfo (BookInfo(..), id_, rev, read_, token, author, title) where


import Control.Lens (makeLensesWith, camelCaseFields, (^.))
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=), Value(Object), object)
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
  }


makeLensesWith camelCaseFields ''BookInfo


instance FromJSON BookInfo where
  parseJSON (Object o) =
    BookInfo <$> o .: "_id" <*> o .: "_rev" <*> o .: "read" <*> o .: "token" <*> o .:? "author" <*> o .:? "title"
  parseJSON _ =
    error "Invalid BookInfo JSON"


instance ToJSON BookInfo where
  toJSON b =
    object $ [ "_id" .= (b ^. id_)
        , "read" .= (b ^. read_)
        , "type" .= ("book" :: Text)
        , "token" .= (b ^. token) ] ++
    catMaybes [ ("_rev" .=) <$> (b ^. rev)
              , ("author" .=) <$> (b ^. author)
              , ("title" .=) <$> (b ^. title) ]
