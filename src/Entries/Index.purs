module Entries.Index where


import Common.Data.ServerEnvironmentInfo (ServerEnvironmentInfo(..), getServerEnvironment)
import Common.Data.UserInfo as U
import Common.OneDriveApi (getUserInfo, UserInfo(UserInfo))
import Common.React (mapProps)
import Components.AjaxLoader.AjaxLoader as AjaxLoader
import Components.BooksDirectory as BooksDirectory
import Components.BookThumbnails as BookThumbnails
import Components.Header as Header
import Control.Error.Util (hoistMaybe)
import Control.Monad.Aff (launchAff, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans (lift)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Either (either)
import Data.Foldable (fold)
import Data.Lens ((^.))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Location (setHref)
import DOM.HTML.Window (location)
import Entries.IndexProps (Props, params, page, defaultProps)
import Global (encodeURIComponent)
import Libs.PouchDB (POUCHDB, PouchDB, newPouchDBOpt, newPouchDB, sync, PouchDBAff)
import Libs.PouchDB.Json (putJson, tryGetJson)
import Network.HTTP.Affjax (AJAX)
import Prelude
import React (ReactClass, transformState, createClass) as R
import React.DOM (div) as R
import Thermite as T
import Web.Cookies (getCookie)


type LoadState =
  { userName :: String
  , onedriveToken :: String
  , db :: PouchDB
  }


type State =
  Maybe LoadState


spec :: forall eff action. T.Spec (ajax :: AJAX, err :: EXCEPTION, pouchdb :: POUCHDB, dom :: DOM | eff) State Json action
spec =
  mapProps convertProps spec'
  where
    convertProps =
      either (const defaultProps) id <<< decodeJson


spec' :: forall eff action. T.Spec (ajax :: AJAX, err :: EXCEPTION, pouchdb :: POUCHDB, dom :: DOM | eff) State Props action
spec' =
  T.withState $ \st ->
                  case st of
                    Nothing ->
                      AjaxLoader.spec
                    Just s ->
                      fold [ mapProps (convertToHeaderProps s) Header.spec
                           , T.simpleSpec T.defaultPerformAction renderBooksDirectory
                           , T.simpleSpec T.defaultPerformAction renderThumbnails
                           ]
  where
    convertToHeaderProps s _ =
      { title: "MyBooks"
      , userName: Just s.userName
      , error: Nothing
      }

    renderBooksDirectory _ _ (Just state) _ =
      [ R.div
        [ ]
        [ BooksDirectory.booksDirectory $ convertToBooksDirectoryProps state ]
      ]
    renderBooksDirectory _ _ _ _ = []

    renderThumbnails _ props (Just s) _ =
      [ BookThumbnails.bookThumbnails
        { db: s.db
        , currentPage: getCurrentPage props
        }
      ]
    renderThumbnails _ _ _ _ =
      []

    getCurrentPage props =
      fromMaybe 0 $ props ^. (params <<< page)

    convertToBooksDirectoryProps s =
      { onedriveToken: s.onedriveToken
      , db: Just s.db
      }


component :: R.ReactClass Json
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
        Just onedriveToken -> void $ launchAff $
          (do
              (ServerEnvironmentInfo serverEnvironment) <- getServerEnvironment
              u@(UserInfo userInfo) <- getUserInfo onedriveToken
              let
                dbName =
                  encodeURIComponent $ "my-books/" <> userInfo._id
                remoteDbName =
                  serverEnvironment.userCouchdbServer <> "/" <> dbName
                globalDbName =
                  serverEnvironment.couchdbServer <> "/my-books"
              localDb <- liftEff $ newPouchDB dbName
              remoteDb <- liftEff $ newPouchDBOpt remoteDbName { ajax: { headers: { "X-Onedrive-Token": onedriveToken } } }
              void $ liftEff $ sync localDb remoteDb { live: true, retry: true }
              globalDb <- liftEff $ newPouchDB globalDbName
              updateUserInfoInDbIfNeeded globalDb u
              let
                newState =
                  { onedriveToken
                  , db : localDb
                  , userName : userInfo.displayName
                  }
              void $ liftEff $ R.transformState this (\_ -> Just newState)) `catchError` handleError

    handleError :: forall e. Error -> Aff (dom :: DOM, console :: CONSOLE | e) Unit
    handleError e = do
      liftEff $ log $ message e
      liftEff $ redirect "/login"


defaultState :: State
defaultState =
  Nothing


redirect :: forall e. String -> Eff (dom :: DOM | e) Unit
redirect url =
  void $ window >>= location >>= setHref url


updateUserInfoInDbIfNeeded :: forall e. PouchDB -> UserInfo -> PouchDBAff e Unit
updateUserInfoInDbIfNeeded db (UserInfo info) = void $ runMaybeT $ do
  maybeUserInfo <- lift $ tryGetJson db info._id
  newUserInfo <- hoistMaybe $ case maybeUserInfo of
    Nothing ->
      Just $ U.UserInfo
      { _id : info._id
      , _rev : Nothing
      , displayName : info.displayName
      }
    Just (U.UserInfo userInfo) ->
      if (userInfo.displayName /= info.displayName)
      then Just $ U.UserInfo userInfo
           { displayName = info.displayName
           }
      else Nothing
  lift $ putJson db newUserInfo
