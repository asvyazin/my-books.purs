{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module BookIndexer.Types.BookInfo (BookInfo(..), id_, rev, read_) where


import Control.Lens (makeLensesWith, camelCaseFields, (^.))
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.=), Value(Object), object)
import Data.Maybe (catMaybes)
import Data.Text (Text)


data BookInfo =
  BookInfo
  { bookInfoId_ :: Text
  , bookInfoRev :: Maybe Text
  , bookInfoRead_ :: Bool
  }


makeLensesWith camelCaseFields ''BookInfo


instance FromJSON BookInfo where
  parseJSON (Object o) =
    BookInfo <$> o .: "_id" <*> o .: "_rev" <*> o .: "read"
  parseJSON _ =
    error "Invalid BookInfo JSON"


instance ToJSON BookInfo where
  toJSON b =
    let
      state =
        [ Just ("_id" .= (b ^. id_))
        , ("_rev" .=) <$> (b ^. rev)
        , Just ("read" .= (b ^. read_))
        , Just ("type" .= ("book" :: Text))
        ]
    in
      object $ catMaybes state
