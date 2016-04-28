module Common.Data.BooksDirectoryInfo where


import Common.Json ((.??), withRev)
import Data.Argonaut.Core (toObject, jsonEmptyObject)
import Data.Argonaut.Combinators ((:=), (?>>=), (.?), (~>))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Maybe (Maybe(Nothing))
import Prelude


newtype BooksDirectoryInfo =
  BooksDirectoryInfo
  { _id :: String
  , _rev :: Maybe String
  , booksItemId :: Maybe String
  }


instance decodeJsonBooksDirectoryInfo :: DecodeJson BooksDirectoryInfo where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    _id <- o .? "_id"
    _rev <- o .?? "_rev"
    booksItemId <- o .? "booksItemId"
    return $ BooksDirectoryInfo { _id, _rev, booksItemId }


instance encodeJsonBooksDirectoryInfo :: EncodeJson BooksDirectoryInfo where
  encodeJson (BooksDirectoryInfo info) =
    ("_id" := info._id) ~> ("booksItemId" := info.booksItemId) ~> jsonEmptyObject `withRev` info._rev


booksDirectoryInfoId :: String
booksDirectoryInfoId = "booksDirectoryInfo"


defaultBooksDirectoryInfo :: BooksDirectoryInfo
defaultBooksDirectoryInfo =
  BooksDirectoryInfo
  { _id : booksDirectoryInfoId
  , _rev : Nothing
  , booksItemId : Nothing
  }
