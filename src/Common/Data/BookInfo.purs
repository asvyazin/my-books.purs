module Common.Data.BookInfo where


import Common.Json ((.??))
import Control.Error.Util (note)
import Data.Argonaut.Core (toObject)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Maybe (Maybe)
import Prelude


newtype BookInfo =
  BookInfo
  { _id :: String
  , _rev :: Maybe String
  , read :: Boolean
  }


instance decodeJsonBookInfo :: DecodeJson BookInfo where
  decodeJson json = do
    o <- note "Expected object" $ toObject json
    _id <- o .? "_id"
    _rev <- o .?? "_rev"
    read <- o .? "read"
    pure $ BookInfo { _id, _rev, read }
