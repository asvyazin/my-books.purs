module Common.Data.BookInfo where


import Control.Error.Util (note)
import Data.Argonaut.Core (toObject)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.?), (.??))
import Data.Maybe (Maybe)
import Prelude


newtype BookInfo =
  BookInfo
  { _id :: String
  , _rev :: Maybe String
  , read :: Boolean
  , author :: Maybe String
  , title :: Maybe String
  , epubVersion :: Maybe String
  , onedriveId :: Maybe String
  }


instance decodeJsonBookInfo :: DecodeJson BookInfo where
  decodeJson json = do
    o <- note "Expected object" $ toObject json
    _id <- o .? "_id"
    _rev <- o .?? "_rev"
    read <- o .? "read"
    author <- o .?? "author"
    title <- o .?? "title"
    epubVersion <- o .?? "epubVersion"
    onedriveId <- o .?? "onedriveId"
    pure $ BookInfo { _id, _rev, read, author, title, epubVersion, onedriveId }
