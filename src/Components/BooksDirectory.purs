module Components.BooksDirectory where


import Common.React
import qualified Components.ChooseDirectoryModal as ChooseDirectoryModal
import Control.Error.Util
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Maybe.Trans
import Data.Foldable
import Data.Lens
import Data.Maybe
import Data.Monoid
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
import qualified Libs.PouchDB as DB


type Props =
  { onedriveToken :: String
  , db :: Maybe DB.PouchDB
  }


type State =
  { directory :: Maybe DirectoryInfo
  , stateLoaded :: Boolean
  , modalState :: ChooseDirectoryModal.State
  }


modalState :: LensP State ChooseDirectoryModal.State
modalState =
  lens _.modalState (_ { modalState = _ })


type DirectoryInfo =
  { itemId :: Maybe String
  , path :: String
  }


type Action =
  ChooseDirectoryModal.Action


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


chooseButton :: forall eff. T.Spec eff State Props Action
chooseButton =
  T.simpleSpec T.defaultPerformAction render
  where
    render :: T.Render State Props Action
    render dispatch _ _ _ =
      let
        buttonProps =
          { bsSize : "large"
          , onClick : dispatch ChooseDirectoryModal.ShowModal
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
        { onClick: dispatch ChooseDirectoryModal.ShowModal
        , bsStyle: "link"
        }
        [ R.text "Choose another" ]
      ]


spec :: forall eff. T.Spec (ajax :: AJAX, err :: EXCEPTION, pouchdb :: DB.POUCHDB | eff) State Props Action
spec =
  fold
  [ T.simpleSpec performAction T.defaultRender
  , withProps (\p ->
                T.withState (\st ->
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
                                , maybe mempty (\db -> mapProps (\pp -> { onedriveToken: pp.onedriveToken, db }) $ T.focusState modalState ChooseDirectoryModal.spec) p.db
                                ]
                            )
              )
  ]
  where
    performAction :: T.PerformAction (ajax :: AJAX, err :: EXCEPTION, pouchdb :: DB.POUCHDB | eff) State Props Action
    performAction (ChooseDirectoryModal.FileTreeAction action) props state update =
      case FileTree.unwrapChildAction action of
        Tuple itemId FileTree.SelectDirectory ->
          launchAff $ do
            directory <- getDirectoryInfo props.onedriveToken itemId
            (liftEff' $ update $ state { directory = Just directory }) >>= guardEither
        _ ->
          pure unit
    performAction _ _ _ _ =
      pure unit


reactClass :: R.ReactClass Props
reactClass =
  R.createClass reactSpec.spec { componentDidMount = componentDidMount }
  where
    reactSpec =
      T.createReactSpec spec initialState

    initialState =
      { directory: Nothing
      , stateLoaded: false
      , modalState: ChooseDirectoryModal.defaultState
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
