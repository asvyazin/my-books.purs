module Common.Settings where


import Control.Monad.Eff.Exception
import Data.Argonaut.Combinators
import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Argonaut.Encode
import Data.Bifunctor
import Data.List
import Data.Maybe
import qualified Data.StrMap as S
import qualified Libs.PouchDB as DB
import Prelude

import Common.Json
import Common.Monad


newtype Settings =
  Settings
  { _id :: String
  , _rev :: Maybe String
  , booksDirectory :: Maybe String
  }


instance decodeJsonSettings :: DecodeJson Settings where
  decodeJson o = do
    jObj <- mFail "Expected object" $ toObject o
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
getSettings db = do
  json <- DB.get db settingsId
  guardEither $ lmap error $ decodeJson json


tryGetSettings :: forall e. DB.PouchDB -> DB.PouchDBAff e (Maybe Settings)
tryGetSettings db = do
  json <- DB.tryGet db settingsId
  case json of
    Nothing ->
      return Nothing
    Just j -> do
      result <- guardEither $ lmap error $ decodeJson j
      return $ Just result


newSettings :: Settings
newSettings =
  Settings
  { _id: settingsId
  , _rev: Nothing
  , booksDirectory: Nothing
  }


updateSettings :: forall e. DB.PouchDB -> (Settings -> Settings) -> DB.PouchDBAff e DB.PutResponse
updateSettings db update = do
  settings <- fromMaybe newSettings <$> tryGetSettings db
  setSettings db $ update settings


setSettings :: forall e. DB.PouchDB -> Settings -> DB.PouchDBAff e DB.PutResponse
setSettings db settings =
  DB.put db $ encodeJson settings
