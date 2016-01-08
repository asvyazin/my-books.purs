module Entries.Index where


import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Nullable
import DOM
import DOM.HTML
import DOM.HTML.Types
import DOM.HTML.Window
import DOM.Node.ParentNode
import qualified Libs.PouchDB as DB
import Network.HTTP.Affjax
import Prelude
import qualified React as R
import Web.Cookies


import Common.Monad
import Common.OneDriveApi
import Entries.Index.Class


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
    let props =
          { onedriveToken
          , db: Just db
          , userName: Just user
          }
    void $ R.render (R.createFactory component props) container
