module Libs.PouchDB.Json where


import Common.Monad (guardEither)
import Control.Error.Util (hoistMaybe)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.List (List)
import Data.Maybe (Maybe)
import Libs.PouchDB as DB
import Libs.PouchDB.QueryItem (QueryItem)
import Prelude


getJson :: forall e a. DecodeJson a => DB.PouchDB -> String -> DB.PouchDBAff e a
getJson db objId = do
  json <- DB.get db objId
  guardEither $ lmap error $ decodeJson json


tryGetJson :: forall e a. DecodeJson a => DB.PouchDB -> String -> DB.PouchDBAff e (Maybe a)
tryGetJson db objId = runMaybeT $ do
  json <- lift (DB.tryGet db objId) >>= hoistMaybe
  guardEither $ lmap error $ decodeJson json


putJson :: forall e a. EncodeJson a => DB.PouchDB -> a -> DB.PouchDBAff e Unit
putJson db obj =
  void $ DB.put db $ encodeJson obj


query :: forall e a options. DecodeJson a => DB.PouchDB -> String -> options -> DB.PouchDBAff e (DB.QueryResult (List (QueryItem a)))
query db index opt = do
  res <- DB.query' db index opt
  newRows <- guardEither $ lmap error $ decodeJson res.rows
  pure { offset: res.offset
       , total_rows: res.total_rows
       , rows: newRows
       }
