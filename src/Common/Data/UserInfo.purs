module Common.Data.UserInfo where


import Data.Argonaut.Core (toObject, jsonEmptyObject)
import Data.Argonaut.Combinators ((:=), (~>), (?>>=), (.?))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Prelude


newtype UserInfo =
  UserInfo
  { _id :: String
  , _rev :: String
  , displayName :: String
  }


instance decodeJsonUserInfo :: DecodeJson UserInfo where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    _id <- o .? "_id"
    _rev <- o .? "_rev"
    displayName <- o .? "displayName"
    return $ UserInfo { _id, _rev, displayName }


instance encodeJsonUserInfo :: EncodeJson UserInfo where
  encodeJson (UserInfo info) =
    ("_id" := info._id) ~> ("_rev" := info._rev) ~> ("displayName" := info.displayName) ~> jsonEmptyObject


userInfoId :: String
userInfoId = "userInfo"
