module Common.Data.BooksDirectoryInfo where


import Data.Argonaut.Core (toObject, fromObject)
import Data.Argonaut.Combinators ((:=), (?>>=), (.?))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.List (toList)
import Data.Maybe (Maybe(Nothing))
import Data.StrMap (fromList) as S
import Prelude


newtype BooksDirectoryInfo =
  BooksDirectoryInfo
  { _id :: String
  , _rev :: String
  , booksItemId :: Maybe String
  }


instance decodeJsonBooksDirectoryInfo :: DecodeJson BooksDirectoryInfo where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    _id <- o .? "_id"
    _rev <- o .? "_rev"
    booksItemId <- o .? "booksItemId"
    return $ BooksDirectoryInfo { _id, _rev, booksItemId }


instance encodeJsonBooksDirectoryInfo :: EncodeJson BooksDirectoryInfo where
  encodeJson (BooksDirectoryInfo info) =
    fromObject $ S.fromList $ toList
    [ "_id" := info._id
    , "_rev" := info._rev
    , "booksItemId" := info.booksItemId
    ]


booksDirectoryInfoId :: String
booksDirectoryInfoId = "booksDirectoryInfo"


defaultBooksDirectoryInfo :: BooksDirectoryInfo
defaultBooksDirectoryInfo =
  BooksDirectoryInfo
  { _id : booksDirectoryInfoId
  , _rev : ""
  , booksItemId : Nothing
  }
