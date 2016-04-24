module Common.Data.ServerEnvironmentInfo where


import Data.Argonaut.Core (toObject)
import Data.Argonaut.Combinators ((?>>=), (.?))
import Data.Argonaut.Decode (class DecodeJson)
import Prelude


newtype ServerEnvironmentInfo =
  ServerEnvironmentInfo
  { appBaseUrl :: String
  , onedriveClientId :: String
  }


instance decodeJsonServerEnvironmentInfo :: DecodeJson ServerEnvironmentInfo where
  decodeJson json = do
    o <- toObject json ?>>= "Expected object"
    appBaseUrl <- o .? "appBaseUrl"
    onedriveClientId <- o .? "onedriveClientId"
    return $ ServerEnvironmentInfo { appBaseUrl, onedriveClientId }
