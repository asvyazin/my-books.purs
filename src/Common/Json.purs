module Common.Json where


import Data.Argonaut.Core (Json, JObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as M
import Prelude


getFieldMaybe :: forall a. (DecodeJson a) => JObject -> String -> Either String (Maybe a)
getFieldMaybe jObj field =
  maybe (pure Nothing) decode $ M.lookup field jObj
  where
    decode json = Just <$> decodeJson json


infix 7 getFieldMaybe as .??


withRev :: Json -> Maybe String -> Json
withRev j rev =
  case rev of
    Nothing ->
      j
    Just r ->
      ("_rev" := r) ~> j
