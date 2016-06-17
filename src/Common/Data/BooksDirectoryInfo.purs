module Common.Data.BooksDirectoryInfo where


import Prelude
import Common.Json ((.??), withRev)
import Data.Argonaut.Core (toObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Nothing), maybe)


newtype BooksDirectoryInfo =
  BooksDirectoryInfo
  { _id :: String
  , _rev :: Maybe String
  , booksItemId :: Maybe String
  }


instance decodeJsonBooksDirectoryInfo :: DecodeJson BooksDirectoryInfo where
  decodeJson json =
    maybe (Left "Expected object") decode $ toObject json
    where
      decode o = do
        _id <- o .? "_id"
        _rev <- o .?? "_rev"
        booksItemId <- o .? "booksItemId"
        pure $ BooksDirectoryInfo { _id, _rev, booksItemId }


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
