{-# LANGUAGE OverloadedStrings #-}
module ServerEnvironmentInfo where


import Data.Aeson (ToJSON(toJSON), object, (.=))
import qualified Data.Text as T


data ServerEnvironmentInfo =
  ServerEnvironmentInfo
  { _appBaseUrl :: T.Text
  , _onedriveClientId :: T.Text
  }


instance ToJSON ServerEnvironmentInfo where
  toJSON (ServerEnvironmentInfo appBaseUrl onedriveClientId) =
    object [ "appBaseUrl" .= appBaseUrl, "onedriveClientId" .= onedriveClientId ]
