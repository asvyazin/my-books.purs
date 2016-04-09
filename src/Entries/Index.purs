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
import Libs.PouchDB as DB
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit, void, ($), (<<<), (<$>), bind, (>>=))
import React (createFactory) as R
import ReactDOM (render) as R
import Web.Cookies (COOKIE, getCookie)


main :: Eff (cookie :: COOKIE, ajax :: AJAX, dom :: DOM, err :: EXCEPTION, pouchdb :: DB.POUCHDB) Unit
main = launchAff $ do
  onedriveToken <-
    liftEff (getCookie "onedriveToken") >>= guardMaybe (error "OneDrive token expected")
  let getName (UserInfo userInfo) =
        userInfo.name
  user <- getName <$> getUserInfo onedriveToken
  liftEff' (renderMain user onedriveToken) >>= guardEither
    

renderMain :: forall e. String -> String -> Eff (dom :: DOM, pouchdb :: DB.POUCHDB, ajax :: AJAX | e) Unit
renderMain user onedriveToken = do
    db <- DB.newPouchDB "MyBooks.purs"
    node <- htmlDocumentToParentNode <$> (window >>= document)
    container <- (fromJust <<< toMaybe) <$> querySelector ".application" node
    let
      props =
        { onedriveToken
        , db: Just db
        , userName: Just user
        }
    void $ R.render (R.createFactory component props) container
