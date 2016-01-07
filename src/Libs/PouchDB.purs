module Libs.PouchDB where


import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Foreign
import Data.Maybe
import Prelude


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


foreign import putFFI :: forall e a. PouchDB -> a -> PouchDBCallbackFFI e PutResponse


put :: forall e a. PouchDB -> a -> PouchDBAff e PutResponse
put db doc =
  makeAff $ putFFI db doc


foreign import postFFI :: forall e a. PouchDB -> a -> PouchDBCallbackFFI e PutResponse


post :: forall e a. PouchDB -> a -> PouchDBAff e PutResponse
post db doc =
  makeAff $ postFFI db doc


foreign import getFFI :: forall e a. PouchDB -> String -> PouchDBCallbackFFI e a


get :: forall e a. PouchDB -> String -> PouchDBAff e a
get db docId =
  makeAff $ getFFI db docId


foreign import tryGetFFI :: forall e. PouchDB -> String -> PouchDBCallbackFFI e Foreign


tryGet :: forall e a. PouchDB -> String -> PouchDBAff e (Maybe a)
tryGet db docId = do
  foreignResult <- makeAff $ tryGetFFI db docId
  return $ if isNull foreignResult
           then Nothing
           else Just $ unsafeFromForeign foreignResult
