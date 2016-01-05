module Entries.Index where


import Control.Bind
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
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


import Entries.Index.Class
import Common.OneDriveApi
import Common.Settings


mFail2 :: forall m a. (MonadError Error m) => String -> Maybe a -> m a
mFail2 err =
  maybe (throwError $ error err) return


main :: Eff (cookie :: COOKIE, ajax :: AJAX, dom :: DOM, err :: EXCEPTION, pouchdb :: DB.POUCHDB) Unit
main = launchAff $ do
  db <- liftEff $ DB.newPouchDB "MyBooks.purs"
  settings <- tryGetSettings db
  onedriveToken <- mFail2 "OneDrive token expected" =<< liftEff (getCookie "onedriveToken")
  let getName (UserInfo userInfo) =
        userInfo.name
  user <- getName <$> getUserInfo onedriveToken
  liftEff $ renderMain user onedriveToken
    

renderMain :: forall e. String -> String -> Eff (dom :: DOM | e) Unit
renderMain user onedriveToken = do
  node <- htmlDocumentToParentNode <$> (window >>= document)
  container <- (fromJust <<< toMaybe) <$> querySelector ".application" node
  void $ R.render (R.createFactory (component $ Just user) { onedriveToken }) container
