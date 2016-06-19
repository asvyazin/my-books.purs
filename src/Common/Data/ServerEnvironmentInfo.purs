module Common.Data.ServerEnvironmentInfo where


import Common.AjaxHelper (doJsonRequest)
import Control.Error.Util (note)
import Control.Monad.Aff (Aff)
import Data.Argonaut.Core (toObject)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Decode (class DecodeJson)
import Data.HTTP.Method (Method(GET))
import Data.Maybe (maybe)
import Network.HTTP.Affjax (AJAX, defaultRequest)
import Prelude


newtype ServerEnvironmentInfo =
  ServerEnvironmentInfo
  { baseUrl :: String
  , onedriveClientId :: String
  , couchdbServer :: String
  }


instance decodeJsonServerEnvironmentInfo :: DecodeJson ServerEnvironmentInfo where
  decodeJson json = do
    o <- note "Expected object" $ toObject json
    baseUrl <- o .? "baseUrl"
    onedriveClientId <- o .? "onedriveClientId"
    couchdbServer <- o .? "couchdbServer"
    pure $ ServerEnvironmentInfo { baseUrl, onedriveClientId, couchdbServer }


getServerEnvironment :: forall e. Aff (ajax :: AJAX | e) ServerEnvironmentInfo
getServerEnvironment =
  doJsonRequest $ defaultRequest
  { method = GET
  , url = "/server-environment"
  }
