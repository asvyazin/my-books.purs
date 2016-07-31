{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.IndexerState where


import BookIndexer.Types.Seq (Seq)
import Control.Lens ((^.), makeLensesWith, camelCaseFields)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.=), object, Value(Object))
import Data.Maybe (catMaybes)
import Data.Text (Text)


indexerStateId :: Text
indexerStateId =
  "indexerState"


data IndexerState =
  IndexerState
  { indexerStateRev :: Maybe Text
  , indexerStateLastSeq :: Seq
  } deriving (Show)


makeLensesWith camelCaseFields ''IndexerState


instance FromJSON IndexerState where
  parseJSON (Object v) =
    IndexerState <$> v .: "_rev" <*> v .: "last_seq"
  parseJSON _ =
    error "Invalid IndexerState JSON"


instance ToJSON IndexerState where
  toJSON indexerState =
    let 
      state =
        [ Just ("_id" .= indexerStateId)
        , formatRev <$> (indexerState ^. rev)
        , Just ("last_seq" .= (indexerState ^. lastSeq))
        ]
      
      formatRev str =
        "_rev" .= str
    in
      object $ catMaybes state
