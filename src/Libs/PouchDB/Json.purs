module Libs.PouchDB.Json where


import Common.Monad (guardEither)
import Control.Error.Util (hoistMaybe)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans (lift)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe)
import Libs.PouchDB as DB
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
putJson db obj = do
  void $ DB.put db $ encodeJson obj
