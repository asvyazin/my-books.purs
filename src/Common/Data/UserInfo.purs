module Common.Data.UserInfo where


import Common.Json ((.??), withRev)
import Data.Argonaut.Core (toObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Either (Either(Left))
import Data.Maybe (Maybe, maybe)
import Prelude


newtype UserInfo =
  UserInfo
  { _id :: String
  , _rev :: Maybe String
  , displayName :: String
  }


instance decodeJsonUserInfo :: DecodeJson UserInfo where
  decodeJson json =
    maybe (Left "Expected object") decode $ toObject json
    where
      decode o = do
        _id <- o .? "_id"
        _rev <- o .?? "_rev"
        displayName <- o .? "displayName"
        pure $ UserInfo { _id, _rev, displayName }


instance encodeJsonUserInfo :: EncodeJson UserInfo where
  encodeJson (UserInfo info) =
    ("_id" := info._id) ~> ("type" := "userInfo") ~> ("displayName" := info.displayName) ~> jsonEmptyObject `withRev` info._rev
