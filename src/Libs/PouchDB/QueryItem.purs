module Libs.PouchDB.QueryItem where


import Control.Error.Util (note)
import Data.Argonaut.Core (toObject)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Prelude


newtype QueryItem a =
  QueryItem
  { id :: String
  , doc :: a
  }


instance queryItemDecodeJson :: DecodeJson a => DecodeJson (QueryItem a) where
  decodeJson json = do
    o <- note "Expected object" $ toObject json
    _id <- o .? "id"
    doc <- o .? "doc"
    pure $ QueryItem { id: _id, doc: doc }


getDoc :: forall a. QueryItem a -> a
getDoc (QueryItem qi) = qi.doc
