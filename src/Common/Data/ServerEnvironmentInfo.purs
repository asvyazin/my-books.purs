module Common.Data.ServerEnvironmentInfo where


import Common.AjaxHelper (doJsonRequest)
import Control.Monad.Aff (Aff)
import Data.Argonaut.Core (toObject)
import Data.Argonaut.Combinators ((?>>=), (.?))
import Data.Argonaut.Decode (class DecodeJson)
import Network.HTTP.Affjax (AJAX, defaultRequest)
import Network.HTTP.Method (Method(GET))
import Prelude


newtype ServerEnvironmentInfo =
  ServerEnvironmentInfo
  { baseUrl :: String
  , onedriveClientId :: String
  , couchdbServer :: String
  }


instance decodeJsonServerEnvironmentInfo :: DecodeJson ServerEnvironmentInfo where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    baseUrl <- o .? "baseUrl"
    onedriveClientId <- o .? "onedriveClientId"
    couchdbServer <- o .? "couchdbServer"
    return $ ServerEnvironmentInfo { baseUrl, onedriveClientId, couchdbServer }


getServerEnvironment :: forall e. Aff (ajax :: AJAX | e) ServerEnvironmentInfo
getServerEnvironment =
  doJsonRequest $ defaultRequest
  { method = GET
  , url = "/server-environment"
  }
