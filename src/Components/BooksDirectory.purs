module Components.BooksDirectory where


import Common.React
import Control.Error.Util
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Maybe.Trans
import Data.Foldable
import Data.Lens
import Data.Maybe
import Data.String
import Data.Tuple
import Network.HTTP.Affjax
import Prelude
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import Common.Monad
import Common.Settings
import Common.OneDriveApi
import qualified Components.AjaxLoader as AjaxLoader
import qualified Components.OneDriveFileTree as FileTree
import qualified Components.Wrappers.Button as Button
import qualified Components.Wrappers.Glyphicon as Glyphicon
import qualified Components.Wrappers.Modal as Modal
import qualified Libs.PouchDB as DB


type Props =
  { onedriveToken :: String
  , db :: Maybe DB.PouchDB
  }


type State =
  { directory :: Maybe DirectoryInfo
  , showModal :: Boolean
  , stateLoaded :: Boolean
  , modalState :: FileTree.State
  }


modalState :: LensP State FileTree.State
modalState =
  lens _.modalState (_ { modalState = _ })


type DirectoryInfo =
  { itemId :: Maybe String
  , path :: String
  }


data Action
  = HideModal
  | ShowModal
  | FileTreeAction FileTree.Action


fileTreeAction :: PrismP Action FileTree.Action
fileTreeAction =
  prism' FileTreeAction getFileTreeAction
  where
    getFileTreeAction (FileTreeAction x) = Just x
    getFileTreeAction _ = Nothing


getDirectoryInfo :: forall e. String -> Maybe String -> Aff (ajax :: AJAX | e) DirectoryInfo
getDirectoryInfo token itemId = do
  item <- getItem <$> getOneDriveItem token itemId
  let parentPath = fromMaybe "" (getPath <$> item.parentReference)
  return { itemId, path: parentPath ++ "/" ++ item.name }
  where
    getItem (OneDriveItem item) = item
    getPath (ItemReference reference) =
      let
        idx = indexOf ":" reference.path
      in
       maybe reference.path (\i -> drop (i + 1) reference.path) idx


wrapMiddle :: forall eff state props action. T.Spec eff state props action -> T.Spec eff state props action
wrapMiddle =
  over T._render wrapRender
  where
    wrapRender origRender dispatch p s c =
      [ R.div
        [ RP.className "col-md-offset-5" ]
        (origRender dispatch p s c)
      ]


wrapModal :: forall eff props. T.Spec eff State props Action -> T.Spec eff State props Action
wrapModal =
  over T._render wrapRender
  where
    wrapRender origRender dispatch p s c =
      [ Modal.modal
        { show: s.showModal
        , onHide: dispatch HideModal
        }
        [ Modal.header
          { closeButton: true }
          [ R.text "Choose directory" ]
        , Modal.body {}
          (origRender dispatch p s c)
        , Modal.footer {}
          [ Button.button
            { onClick: dispatch HideModal }
            [ R.text "Close" ]
          ]
        ]
      ]


chooseButton :: forall eff. T.Spec eff State Props Action
chooseButton =
  T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render State Props Action
    render dispatch _ _ _ =
      let
        buttonProps =
          { bsSize : "large"
          , onClick : dispatch ShowModal
          }
      in
       [ Button.button buttonProps
         [ R.div
           [ RP.className "col-md-2" ]
           [ Glyphicon.glyphicon' "book"
           , R.text " Choose books directory..."
           ]
         ]
       ]


choosedDirectory :: forall eff state props. DirectoryInfo -> T.Spec eff state props Action
choosedDirectory directory =
  T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render state props Action
    render dispatch _ _ _ =
      [ R.span
        [ RP.className "default" ]
        [ R.text directory.path ]
      , Button.button
        { onClick: dispatch ShowModal
        , bsStyle: "link"
        }
        [ R.text "Choose another" ]
      ]


spec :: forall eff. T.Spec (ajax :: AJAX, err :: EXCEPTION, pouchdb :: DB.POUCHDB | eff) State Props Action
spec =
  T.simpleSpec performAction T.defaultRender
  <> T.withState (\st ->
    if not st.stateLoaded
    then
      AjaxLoader.spec
    else
      fold
      [ wrapMiddle $ case st.directory of
           Nothing ->
             chooseButton
           Just directory ->
             choosedDirectory directory
      , wrapModal $ mapProps (\p ->
                               { name: "root"
                               , onedriveToken: p.onedriveToken
                               , itemId: Nothing
                               , key: "root"
                               }) $ T.focus modalState fileTreeAction FileTree.spec
      ])
  where
    performAction :: T.PerformAction (ajax :: AJAX, err :: EXCEPTION, pouchdb :: DB.POUCHDB | eff) State Props Action
    performAction HideModal _ state update =
      update $ state { showModal = false }

    performAction ShowModal _ state update =
      update $ state { showModal = true }

    performAction (FileTreeAction action) props state update =
      case FileTree.unwrapChildAction action of
        Tuple itemId FileTree.SelectDirectory ->
          launchAff $ do
            directory <- getDirectoryInfo props.onedriveToken itemId
            maybe (return unit) (\db -> void $ updateSettings db (\(Settings s) -> Settings (s { booksDirectory = itemId }))) props.db
            (liftEff' $ update $ state { showModal = false, directory = Just directory }) >>= guardEither
        _ ->
          pure unit


reactClass :: R.ReactClass Props
reactClass =
  R.createClass reactSpec.spec { componentDidMount = componentDidMount }
  where
    reactSpec =
      T.createReactSpec spec initialState

    initialState =
      { directory: Nothing
      , showModal: false
      , stateLoaded: false
      , modalState: FileTree.defaultState
      }

    componentDidMount this = launchAff $ do
      props <- liftEff $ R.getProps this
      void $ runMaybeT $ do
        db <- hoistMaybe props.db
        settings <- MaybeT $ tryGetSettings db
        directory <- lift $ getDirectoryInfo props.onedriveToken $ getBooksDirectory settings
        lift $ liftEff $ R.transformState this (_ { directory = Just directory, stateLoaded = true })
      where
        getBooksDirectory (Settings s) = s.booksDirectory


booksDirectory :: Props -> R.ReactElement
booksDirectory props =
  R.createElement reactClass props []
