module Common.Json where


import Data.Argonaut.Core
import Data.Argonaut.Decode
import Data.Either
import Data.Maybe
import Debug.Trace
import qualified Data.StrMap as M
import Prelude


mFail :: forall a b. a -> Maybe b -> Either a b
mFail err =
  maybe (Left err) Right


(.??) :: forall a. (DecodeJson a) => JObject -> String -> Either String (Maybe a)
(.??) jObj field =
  maybe (pure Nothing) decode $ M.lookup field jObj
  where
    decode json = Just <$> decodeJson json
