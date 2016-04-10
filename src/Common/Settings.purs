module Common.Settings where


import Common.Json ((.??))
import Data.Argonaut.Combinators ((?>>=), (.?), (:=))
import Data.Argonaut.Core (fromObject, toObject)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.List (toList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap as S
import Libs.PouchDB (PouchDB, PouchDBAff) as DB
import Libs.PouchDB.Json (getJson, tryGetJson, putJson) as DB
import Prelude


newtype Settings =
  Settings
  { _id :: String
  , _rev :: Maybe String
  , booksDirectory :: Maybe String
  }


instance decodeJsonSettings :: DecodeJson Settings where
  decodeJson o = do
    jObj <- toObject o ?>>= "Expected object"
    _id <- jObj .? "_id"
    _rev <- jObj .?? "_rev"
    booksDirectory <- jObj .?? "booksDirectory"
    return $ Settings { _id, _rev, booksDirectory }


instance encodeJsonSettings :: EncodeJson Settings where
  encodeJson (Settings s) =
    fromObject $ S.fromList $ toList
    [ "_id" := s._id
    , "_rev" := s._rev
    , "booksDirectory" := s.booksDirectory
    ]


settingsId :: String
settingsId = "settings"


getSettings :: forall e. DB.PouchDB -> DB.PouchDBAff e Settings
getSettings db =
  DB.getJson db settingsId


tryGetSettings :: forall e. DB.PouchDB -> DB.PouchDBAff e (Maybe Settings)
tryGetSettings db =
  DB.tryGetJson db settingsId


newSettings :: Settings
newSettings =
  Settings
  { _id: settingsId
  , _rev: Nothing
  , booksDirectory: Nothing
  }


updateSettings :: forall e. DB.PouchDB -> (Settings -> Settings) -> DB.PouchDBAff e Unit
updateSettings db update = do
  settings <- fromMaybe newSettings <$> tryGetSettings db
  setSettings db $ update settings


setSettings :: forall e. DB.PouchDB -> Settings -> DB.PouchDBAff e Unit
setSettings db settings =
  DB.putJson db settings
