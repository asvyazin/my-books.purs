module Common.Json where


import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Either
import Data.Maybe
import qualified Data.StrMap as M
import Prelude


(.??) :: forall a. (DecodeJson a) => JObject -> String -> Either String (Maybe a)
(.??) jObj field =
  maybe (pure Nothing) decode $ M.lookup field jObj
  where
    decode json = Just <$> decodeJson json
