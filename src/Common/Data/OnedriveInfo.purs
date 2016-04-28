module Common.Data.OnedriveInfo where


import Common.Json ((.??), withRev)
import Data.Argonaut.Core (toObject, jsonEmptyObject)
import Data.Argonaut.Combinators ((:=), (?>>=), (.?), (~>))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Maybe (Maybe(Nothing))
import Prelude


newtype OnedriveInfo =
  OnedriveInfo
  { _id :: String
  , _rev :: Maybe String
  , token :: Maybe String
  }


instance decodeJsonOnedriveInfo :: DecodeJson OnedriveInfo where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    _id <- o .? "_id"
    _rev <- o .?? "_rev"
    token <- o .?? "token"
    return $ OnedriveInfo { _id, _rev, token }


instance encodeJsonOnedriveInfo :: EncodeJson OnedriveInfo where
  encodeJson (OnedriveInfo info) =
    ("_id" := info._id) ~> ("token" := info.token) ~> jsonEmptyObject `withRev` info._rev


onedriveInfoId :: String
onedriveInfoId = "onedriveInfo"


defaultOnedriveInfo :: OnedriveInfo
defaultOnedriveInfo =
  OnedriveInfo
  { _id : onedriveInfoId
  , _rev : Nothing
  , token : Nothing
  }
