module Common.AjaxHelper where


import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, affjax)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude


doJsonRequest :: forall e a. (DecodeJson a) => AffjaxRequest Unit -> Aff (ajax :: AJAX | e) a
doJsonRequest req =
  affjax req >>= getJson


getJson :: forall m a. DecodeJson a => MonadError Error m => AffjaxResponse String -> m a
getJson resp = do
  when (resp.status /= StatusCode 200) $
    throwError $ error "AJAX request failed"
  let result = jsonParser resp.response >>= decodeJson
  either (throwError <<< error) pure result
