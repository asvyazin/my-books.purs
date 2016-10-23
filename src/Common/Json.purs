module Common.Json where


import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Maybe (Maybe(Nothing, Just))


withRev :: Json -> Maybe String -> Json
withRev j rev =
  case rev of
    Nothing ->
      j
    Just r ->
      ("_rev" := r) ~> j
