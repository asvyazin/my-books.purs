module Common.Data.ServerEnvironmentInfo where


import Common.AjaxHelper (doJsonRequest)
import Control.Error.Util (note)
import Control.Monad.Aff (Aff)
import Data.Argonaut.Core (toObject)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Decode (class DecodeJson)
import Data.Either (Either(Left))
import Data.HTTP.Method (Method(GET))
import Network.HTTP.Affjax (AJAX, defaultRequest)
import Prelude


newtype ServerEnvironmentInfo =
  ServerEnvironmentInfo
  { baseUrl :: String
  , onedriveClientId :: String
  , couchdbServer :: String
  , userCouchdbServer :: String
  }


instance decodeJsonServerEnvironmentInfo :: DecodeJson ServerEnvironmentInfo where
  decodeJson json = do
    o <- note "Expected object" $ toObject json
    baseUrl <- o .? "baseUrl"
    onedriveClientId <- o .? "onedriveClientId"
    couchdbServer <- o .? "couchdbServer"
    userCouchdbServer <- o .? "userCouchdbServer"
    pure $ ServerEnvironmentInfo { baseUrl, onedriveClientId, couchdbServer, userCouchdbServer }


getServerEnvironment :: forall e. Aff (ajax :: AJAX | e) ServerEnvironmentInfo
getServerEnvironment =
  doJsonRequest $ defaultRequest
  { method = Left GET
  , url = "/server-environment"
  }
