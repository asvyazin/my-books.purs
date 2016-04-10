module Common.Data.OnedriveInfo where


import Common.Json ((.??))
import Data.Argonaut.Core (toObject)
import Data.Argonaut.Combinators ((:=), (~>), (?>>=), (.?))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Maybe (Maybe(Nothing))
import Prelude


newtype OnedriveInfo =
  OnedriveInfo
  { _id :: String
  , _rev :: String
  , token :: Maybe String
  }


instance decodeJsonOnedriveInfo :: DecodeJson OnedriveInfo where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    _id <- o .? "_id"
    _rev <- o .? "_rev"
    token <- o .?? "token"
    return $ OnedriveInfo { _id, _rev, token }


instance encodeJsonOnedriveInfo :: EncodeJson OnedriveInfo where
  encodeJson (OnedriveInfo info) =
    ("_id" := info._id) ~> ("_rev" := info._rev) ~> ("token" := info.token)


onedriveInfoId :: String
onedriveInfoId = "onedriveInfoId"


defaultOnedriveInfo :: OnedriveInfo
defaultOnedriveInfo =
  OnedriveInfo
  { _id : onedriveInfoId
  , _rev : ""
  , token : Nothing
  }
