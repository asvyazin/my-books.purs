{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module BookIndexer.Environment where


import Common.ServerEnvironmentInfo (ServerEnvironmentInfo)
import Control.Lens (makeLensesWith, camelCaseFields)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager(getHttpManager))


data Environment =
  Environment
  { environmentManager :: Manager
  , environmentServerEnvironment :: ServerEnvironmentInfo
  }


makeLensesWith camelCaseFields ''Environment


instance HasHttpManager Environment where
  getHttpManager = environmentManager
