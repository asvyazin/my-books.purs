module Entries.Index where


import Common.Monad (guardEither, guardMaybe)
import Common.OneDriveApi (UserInfo(..), getUserInfo)
import Control.Monad.Aff (liftEff', launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import Entries.Index.Class (component)
import Global (encodeURIComponent)
import Libs.PouchDB as DB
import Network.HTTP.Affjax (AJAX)
import Prelude
import React (createFactory) as R
import ReactDOM (render) as R
import Web.Cookies (COOKIE, getCookie)


main :: Eff (cookie :: COOKIE, ajax :: AJAX, dom :: DOM, err :: EXCEPTION, pouchdb :: DB.POUCHDB) Unit
main = launchAff $ do
  onedriveToken <-
    liftEff (getCookie "onedriveToken") >>= guardMaybe (error "OneDrive token expected")
  UserInfo userInfo <- getUserInfo onedriveToken
  let
    dbName =
      encodeURIComponent $ "my-books/" ++ userInfo.id
    remoteDb =
      "http://localhost:5984/" ++ dbName
  localDb <- liftEff $ DB.newPouchDB dbName
  void $ liftEff $ DB.sync localDb remoteDb { live: true, retry: true }
  liftEff' (renderMain userInfo.name onedriveToken localDb) >>= guardEither


renderMain :: forall e. String -> String -> DB.PouchDB -> Eff (dom :: DOM, pouchdb :: DB.POUCHDB, ajax :: AJAX | e) Unit
renderMain user onedriveToken db = do
    node <- htmlDocumentToParentNode <$> (window >>= document)
    container <- (fromJust <<< toMaybe) <$> querySelector ".application" node
    let
      props =
        { onedriveToken
        , db: Just db
        , userName: Just user
        }
    void $ R.render (R.createFactory component props) container
