module Entries.Index where


import Control.Bind
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Data.Either
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


main :: Eff (cookie :: COOKIE, ajax :: AJAX, dom :: DOM, err :: EXCEPTION, pouchdb :: DB.POUCHDB) Unit
main = launchAff $ do
  db <- liftEff $ DB.newPouchDB "MyBooks.purs"
  settings <- tryGetSettings db
  onedriveToken <- liftEff $ getCookie "onedriveToken"
  let getName (UserInfo userInfo) =
        userInfo.name
  let fromEither =
        eitherToMaybe >=> (return <<< getName)
  let maybeName =
        getUserInfo >=> (return <<< fromEither)
  user <- maybe (return Nothing) maybeName onedriveToken
  liftEff $ renderMain user $ fromJust onedriveToken
    

eitherToMaybe :: forall a b. Either a b -> Maybe b
eitherToMaybe =
  either (const Nothing) Just


renderMain :: forall e. Maybe String -> String -> Eff (dom :: DOM | e) Unit
renderMain user onedriveToken = do
  node <- htmlDocumentToParentNode <$> (window >>= document)
  container <- (fromJust <<< toMaybe) <$> querySelector ".application" node
  void $ R.render (R.createFactory (component user) { onedriveToken }) container
