module Entries.Index where


import Common.Data.OnedriveInfo (OnedriveInfo(..), defaultOnedriveInfo, onedriveInfoId)
import Common.Data.ServerEnvironmentInfo (ServerEnvironmentInfo(..), getServerEnvironment)
import Common.Data.UserInfo as U
import Common.OneDriveApi (getUserInfo, UserInfo(UserInfo))
import Common.React (mapProps)
import Components.AjaxLoader as AjaxLoader
import Components.BooksDirectory as BooksDirectory
import Components.Header as Header
import Control.Monad (when)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (setHref)
import DOM.HTML.Window (location)
import Global (encodeURIComponent)
import Libs.PouchDB (POUCHDB, PouchDB, newPouchDB, sync, PouchDBAff)
import Libs.PouchDB.Json (putJson, tryGetJson)
import Network.HTTP.Affjax (AJAX)
import Prelude
import React as R
import Thermite as T
import Web.Cookies (getCookie)


type LoadState =
  { userName :: String
  , onedriveToken :: String
  , db :: PouchDB
  }


type State =
  Maybe LoadState


spec :: forall eff props action. T.Spec (ajax :: AJAX, err :: EXCEPTION, pouchdb :: POUCHDB | eff) State props action
spec =
  T.withState $ \st ->
                  case st of
                    Nothing ->
                      AjaxLoader.spec
                    Just s ->
                      fold [ mapProps (convertToHeaderProps s) Header.spec
                           , T.simpleSpec T.defaultPerformAction renderBooksDirectory
                           ]
  where
    convertToHeaderProps s _ =
      { title: "MyBooks"
      , userName: Just s.userName
      , error: Nothing
      }

    renderBooksDirectory _ _ (Just state) _ =
      [ BooksDirectory.booksDirectory $ convertToBooksDirectoryProps state ]
    renderBooksDirectory _ _ _ _ = []

    convertToBooksDirectoryProps s =
      { onedriveToken: s.onedriveToken
      , db: Just s.db
      }


component :: forall props. R.ReactClass props
component =
  R.createClass reactSpec.spec { componentDidMount = componentDidMount }
  where
    reactSpec =
      T.createReactSpec spec defaultState

    componentDidMount this = do
      maybeOnedriveToken <- getCookie "onedriveToken"
      case maybeOnedriveToken of
        Nothing ->
          redirect "/login"
        Just onedriveToken -> launchAff $ do
          (ServerEnvironmentInfo serverEnvironment) <- getServerEnvironment
          u@(UserInfo userInfo) <- getUserInfo onedriveToken
          let
            dbName =
              encodeURIComponent $ "my-books/" ++ userInfo._id
            remoteDbName =
              serverEnvironment.couchdbServer ++ "/" ++ dbName
          localDb <- liftEff $ newPouchDB dbName
          remoteDb <- liftEff $ newPouchDB remoteDbName
          void $ liftEff $ sync localDb remoteDb { live: true, retry: true }
          updateOneDriveInfoInDbIfNeeded localDb onedriveToken
          updateUserInfoInDbIfNeeded localDb u
          liftEff $ R.transformState this (\_ -> Just { onedriveToken, db : localDb, userName : userInfo.displayName })


defaultState :: State
defaultState =
  Nothing


redirect :: forall e. String -> Eff (dom :: DOM | e) Unit
redirect url =
  void $ window >>= location >>= setHref url


updateOneDriveInfoInDbIfNeeded :: forall e. PouchDB -> String -> PouchDBAff e Unit
updateOneDriveInfoInDbIfNeeded db onedriveToken = do
  OnedriveInfo onedriveInfo <- fromMaybe defaultOnedriveInfo <$> tryGetJson db onedriveInfoId
  when (onedriveInfo.token /= Just onedriveToken) $ putJson db $ OnedriveInfo $ onedriveInfo { token = Just onedriveToken }


updateUserInfoInDbIfNeeded :: forall e. PouchDB -> UserInfo -> PouchDBAff e Unit
updateUserInfoInDbIfNeeded db (UserInfo info) = do
  maybeUserInfo <- tryGetJson db U.userInfoId
  case maybeUserInfo of
    Nothing -> do
      let
        newUserInfo =
          U.UserInfo
          { _id : U.userInfoId
          , _rev : ""
          , displayName : info.displayName
          }
      putJson db newUserInfo
    Just (U.UserInfo userInfo) ->
      when (userInfo.displayName /= info.displayName) $ do
        let
          newUserInfo =
            U.UserInfo userInfo
            { displayName = info.displayName
            }
        putJson db newUserInfo
