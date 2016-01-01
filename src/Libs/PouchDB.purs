module Libs.PouchDB where


import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe
import Prelude


readForeignAff :: forall e a. (IsForeign a) => Foreign -> Aff e a
readForeignAff fr =
  case read fr of
    Left (JSONError msg) ->
      throwError $ error msg
    Left _ ->
      throwError $ error "Unknown error"
    Right result ->
      return result


foreign import data POUCHDB :: !


foreign import data PouchDB :: *


type PouchDBEff e a =
  Eff (pouchdb :: POUCHDB | e) a


type PouchDBAff e a =
  Aff (pouchdb :: POUCHDB | e) a


type PouchDBCallbackFFI e a =
  (Error -> PouchDBEff e Unit) -> (a -> PouchDBEff e Unit) -> PouchDBEff e Unit


foreign import newPouchDB :: forall e. String -> PouchDBEff e PouchDB


type PutResponse =
  { ok :: Boolean
  , id :: String
  , rev :: String
  }


data PutResponseFFI = PutResponseFFI PutResponse


instance putResponseIsForeign :: IsForeign PutResponseFFI where
  read o = do
    okVal <- readProp "ok" o
    idVal <- readProp "id" o
    revVal <- readProp "rev" o
    return $ PutResponseFFI
      { ok: okVal
      , id: idVal
      , rev: revVal
      }


foreign import putFFI :: forall e. PouchDB -> String -> Maybe String -> Foreign -> PouchDBCallbackFFI e Foreign


put :: forall a e. PouchDB -> String -> Maybe String -> a -> PouchDBAff e PutResponse
put db docId docRev doc = do
  foreignResult <- makeAff $ putFFI db docId docRev $ toForeign doc
  (PutResponseFFI result) <- readForeignAff foreignResult
  return result


foreign import postFFI :: forall e. PouchDB -> Foreign -> PouchDBCallbackFFI e Foreign


post :: forall a e. PouchDB -> a -> PouchDBAff e PutResponse
post db doc = do
  foreignResult <- makeAff $ postFFI db $ toForeign doc
  (PutResponseFFI result) <- readForeignAff foreignResult
  return result


foreign import getFFI :: forall e. PouchDB -> String -> PouchDBCallbackFFI e Foreign


get :: forall a e. (IsForeign a)  => PouchDB -> String -> PouchDBAff e a
get db docId =
  makeAff (getFFI db docId) >>= readForeignAff


tryGet :: forall a e. (IsForeign a) => PouchDB -> String -> PouchDBAff e (Maybe a)
tryGet db docId = do
  result <- attempt $ get db docId
  return $ either (const Nothing) Just result
