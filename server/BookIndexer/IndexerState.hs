{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module BookIndexer.IndexerState where


import Control.Lens ((^.), makeLensesWith, camelCaseFields)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.=), object, Value(Object))
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)


indexerStateId :: Text
indexerStateId =
  "indexerState"


data IndexerState =
  IndexerState
  { indexerStateRev :: Maybe String
  , indexerStateLastSeq :: Int64
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
