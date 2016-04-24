module Entries.Login where

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import Entries.Login.Class (component)
import Network.HTTP.Affjax (AJAX)
import Prelude
import React (createFactory) as R
import ReactDOM (render) as R
import Common.Data.ServerEnvironmentInfo (ServerEnvironmentInfo(..), getServerEnvironment)


main :: Eff (dom :: DOM, ajax :: AJAX, err :: EXCEPTION) Unit
main = launchAff $ do
  node <- liftEff $ htmlDocumentToParentNode <$> (window >>= document)
  container <- liftEff $ (fromJust <<< toMaybe) <$> querySelector ".application" node
  (ServerEnvironmentInfo serverEnvironment) <- getServerEnvironment
  let
    props =
      { scope : "wl.signin onedrive.readonly"
      , clientId : serverEnvironment.onedriveClientId
      , appBaseUrl : serverEnvironment.appBaseUrl
      }
  liftEff $ void $ R.render (R.createFactory component props) container
