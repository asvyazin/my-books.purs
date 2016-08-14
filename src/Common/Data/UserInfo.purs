module Common.Data.UserInfo where


import Common.Json (withRev)
import Control.Error.Util (note)
import Data.Argonaut.Core (toObject, jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.?), (.??))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Maybe (Maybe)
import Prelude


newtype UserInfo =
  UserInfo
  { _id :: String
  , _rev :: Maybe String
  , displayName :: String
  }


instance decodeJsonUserInfo :: DecodeJson UserInfo where
  decodeJson json = do
    o <- note "Expected object" $ toObject json
    _id <- o .? "_id"
    _rev <- o .?? "_rev"
    displayName <- o .? "displayName"
    pure $ UserInfo { _id, _rev, displayName }


instance encodeJsonUserInfo :: EncodeJson UserInfo where
  encodeJson (UserInfo info) =
    ("_id" := info._id) ~> ("type" := "userInfo") ~> ("displayName" := info.displayName) ~> jsonEmptyObject `withRev` info._rev
