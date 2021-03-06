module Libs.PouchDB where


import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (Error)
import Data.Foreign (Foreign, unsafeFromForeign, isNull)
import Data.Maybe (Maybe(..))
import Prelude


foreign import data POUCHDB :: Effect


foreign import data PouchDB :: Type


type PouchDBEff e a =
  Eff (pouchdb :: POUCHDB | e) a


type PouchDBAff e a =
  Aff (pouchdb :: POUCHDB | e) a


type PouchDBCallbackFFI e a =
  (Error -> PouchDBEff e Unit) -> (a -> PouchDBEff e Unit) -> PouchDBEff e Unit


foreign import newPouchDB :: forall e. String -> PouchDBEff e PouchDB


foreign import newPouchDBOpt :: forall e options. String -> options -> PouchDBEff e PouchDB


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
  pure $ if isNull foreignResult
         then Nothing
         else Just $ unsafeFromForeign foreignResult


foreign import data PouchDBSync :: Type


foreign import sync :: forall srcType destType optionsType e. srcType -> destType -> optionsType -> PouchDBEff e PouchDBSync


foreign import cancel :: forall e. PouchDBSync -> PouchDBEff e Unit


foreign import debugEnable :: forall e. String -> PouchDBEff e Unit


type QueryResult x =
  { offset :: Int
  , rows :: x
  , total_rows :: Int
  }


foreign import queryFFI :: forall e a1 options. PouchDB -> String -> options -> PouchDBCallbackFFI e (QueryResult a1)


query' :: forall e a2 options. PouchDB -> String -> options -> PouchDBAff e (QueryResult a2)
query' db index opt =
  makeAff $ queryFFI db index opt

