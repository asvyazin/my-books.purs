module Entries.Index where


import Common.Data.OnedriveInfo (OnedriveInfo(OnedriveInfo), defaultOnedriveInfo, onedriveInfoId)
import Common.Data.UserInfo (UserInfo(UserInfo), userInfoId) as U
import Common.Monad (guardEither, guardMaybe)
import Common.OneDriveApi (getUserInfo, UserInfo(UserInfo))
import Control.Monad (when)
import Control.Monad.Aff (liftEff', launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import Entries.Index.Class (component)
import Global (encodeURIComponent)
import Libs.PouchDB (POUCHDB, PouchDB, PouchDBAff, sync, newPouchDB) as DB
import Libs.PouchDB.Json (putJson, tryGetJson) as DB
import Network.HTTP.Affjax (AJAX)
import Prelude
import React (createFactory) as R
import ReactDOM (render) as R
import Web.Cookies (COOKIE, getCookie)


main :: Eff (cookie :: COOKIE, ajax :: AJAX, dom :: DOM, err :: EXCEPTION, pouchdb :: DB.POUCHDB) Unit
main = launchAff $ do
  onedriveToken <-
    liftEff (getCookie "onedriveToken") >>= guardMaybe (error "OneDrive token expected")
  u@(UserInfo userInfo) <- getUserInfo onedriveToken
  let
    dbName =
      encodeURIComponent $ "my-books/" ++ userInfo._id
    remoteDb =
      "http://localhost:5984/" ++ dbName
  localDb <- liftEff $ DB.newPouchDB dbName
  void $ liftEff $ DB.sync localDb remoteDb { live: true, retry: true }
  updateOneDriveInfoInDbIfNeeded localDb onedriveToken
  updateUserInfoInDbIfNeeded localDb u
  liftEff' (renderMain userInfo.displayName onedriveToken localDb) >>= guardEither


updateOneDriveInfoInDbIfNeeded :: forall e. DB.PouchDB -> String -> DB.PouchDBAff e Unit
updateOneDriveInfoInDbIfNeeded db onedriveToken = do
  OnedriveInfo onedriveInfo <- fromMaybe defaultOnedriveInfo <$> DB.tryGetJson db onedriveInfoId
  when (onedriveInfo.token /= Just onedriveToken) $ DB.putJson db $ OnedriveInfo $ onedriveInfo { token = Just onedriveToken }


updateUserInfoInDbIfNeeded :: forall e. DB.PouchDB -> UserInfo -> DB.PouchDBAff e Unit
updateUserInfoInDbIfNeeded db (UserInfo info) = do
  maybeUserInfo <- DB.tryGetJson db U.userInfoId
  case maybeUserInfo of
    Nothing -> do
      let
        newUserInfo =
          U.UserInfo
          { _id : U.userInfoId
          , _rev : ""
          , displayName : info.displayName
          }
      DB.putJson db newUserInfo
    Just (U.UserInfo userInfo) ->
      when (userInfo.displayName /= info.displayName) $ do
        let
          newUserInfo =
            U.UserInfo userInfo
            { displayName = info.displayName
            }
        DB.putJson db newUserInfo


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
