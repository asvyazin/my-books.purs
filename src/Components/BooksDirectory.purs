module Components.BooksDirectory (booksDirectory) where


import Common.Monad
import Common.OneDriveApi
import Common.React
import Common.Settings
import qualified Components.AjaxLoader as AjaxLoader
import qualified Components.ChooseDirectoryModal as ChooseDirectoryModal
import qualified Components.OneDriveFileTree as FileTree
import Control.Monad.Aff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Maybe.Trans
import Control.Error.Util
import Data.Foldable
import Data.Lens
import Data.Maybe
import qualified Data.String as S
import Data.Tuple
import Network.HTTP.Affjax
import Prelude
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import qualified Components.Wrappers.Button as Button
import qualified Components.Wrappers.Glyphicon as Glyphicon
import qualified Libs.PouchDB as DB


type Props =
  { onedriveToken :: String
  , db :: Maybe DB.PouchDB
  }


type State =
  { modalState :: ChooseDirectoryModal.State
  , stateLoaded :: Boolean
  , directory :: Maybe DirectoryInfo
  }


modalState :: LensP State ChooseDirectoryModal.State
modalState =
  lens _.modalState (_ { modalState = _ })


directory :: LensP State (Maybe DirectoryInfo)
directory =
  lens _.directory (_ { directory = _ })


defaultState :: State
defaultState =
  { modalState: ChooseDirectoryModal.defaultState
  , stateLoaded: false
  , directory: Nothing
  }


type Action =
  ChooseDirectoryModal.Action


type DirectoryInfo =
  { itemId :: Maybe String
  , path :: String
  }


type ChoosedDirectoryProps =
  { directory :: DirectoryInfo
  , onedriveToken :: String
  , db :: DB.PouchDB
  }


choosedDirectory :: forall eff. T.Spec (ajax :: AJAX, err :: EXCEPTION, pouchdb :: DB.POUCHDB | eff) State ChoosedDirectoryProps Action
choosedDirectory =
  wrapMiddle $ T.simpleSpec T.defaultPerformAction render
  where
    render dispatch props _ _ =
      [ R.span
        [ RP.className "default" ]
        [ R.text props.directory.path ]
      , Button.button
        { onClick: dispatch ChooseDirectoryModal.ShowModal
        , bsStyle: "link"
        }
        [ R.text "Choose another" ]
      ]

    convertProps p =
      { onedriveToken: p.onedriveToken
      , db: p.db
      }


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
  wrapMiddle $ T.simpleSpec T.defaultPerformAction render
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


spec :: forall eff. T.Spec (ajax :: AJAX, err :: EXCEPTION, pouchdb :: DB.POUCHDB | eff) State Props Action
spec =
  fold
  [ T.withState (\st ->
                  if not st.stateLoaded
                  then AjaxLoader.spec
                  else
                    case st.directory of
                      Nothing ->
                        chooseButton
                      Just _ ->
                        mapPropsWithState tryGetChoosedDirectoryProps $ maybeProps choosedDirectory
                )
  , mapProps tryGetChooseDirectoryModalProps $ maybeProps $ T.focusState modalState ChooseDirectoryModal.spec
  ]
  where
    tryGetChoosedDirectoryProps p s = do
      dir <- s.directory
      db <- p.db
      return { onedriveToken: p.onedriveToken
             , directory: dir
             , db
             }

    tryGetChooseDirectoryModalProps p = do
      db <- p.db
      return { onedriveToken: p.onedriveToken
             , db
             }

    performAction (ChooseDirectoryModal.FileTreeAction action) props state update =
      processFileTreeAction action props state update
    performAction _ _ _ _ =
      pure unit

    processFileTreeAction action props state update =
      case FileTree.unwrapChildAction action of
        Tuple itemId FileTree.SelectDirectory ->
          launchAff $ do
            dir <- getDirectoryInfo props.onedriveToken itemId
            (liftEff' $ update $ state { directory = Just dir }) >>= guardEither
        _ ->
          pure unit


reactClass :: R.ReactClass Props
reactClass =
  R.createClass reactSpec.spec { componentDidMount = componentDidMount }
  where
    reactSpec =
      T.createReactSpec spec defaultState

    componentDidMount this = launchAff $ do
      props <- liftEff $ R.getProps this
      void $ runMaybeT $ do
        db <- hoistMaybe props.db
        settings <- MaybeT $ tryGetSettings db
        dir <- lift $ getDirectoryInfo props.onedriveToken $ getBooksDirectory settings
        liftEff $ R.transformState this (_ { directory = Just dir, stateLoaded = true })
      where
        getBooksDirectory (Settings s) = s.booksDirectory


booksDirectory :: Props -> R.ReactElement
booksDirectory props =
  R.createElement reactClass props []


getDirectoryInfo :: forall e. String -> Maybe String -> Aff (ajax :: AJAX | e) DirectoryInfo
getDirectoryInfo token itemId = do
  item <- getItem <$> getOneDriveItem token itemId
  let parentPath = fromMaybe "" (getPath <$> item.parentReference)
  return { itemId, path: parentPath ++ "/" ++ item.name }
  where
    getItem (OneDriveItem item) = item
    getPath (ItemReference reference) =
      let
        idx = S.indexOf ":" reference.path
      in
       maybe reference.path (\i -> S.drop (i + 1) reference.path) idx
