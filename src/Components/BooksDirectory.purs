module Components.BooksDirectory (booksDirectory) where


import Common.Data.BooksDirectoryInfo (BooksDirectoryInfo(BooksDirectoryInfo), booksDirectoryInfoId, defaultBooksDirectoryInfo)
import Common.OneDriveApi (ItemReference(..), OneDriveItem(..), getOneDriveItem)
import Common.React (maybeProps, mapProps, mapPropsWithState)
import Components.AjaxLoader.AjaxLoader as AjaxLoader
import Components.ChoosedDirectory as ChoosedDirectory
import Components.ChooseDirectoryModal as ChooseDirectoryModal
import Components.OneDriveFileTree as FileTree
import Components.Wrappers.Button as Button
import Components.Wrappers.Glyphicon as Glyphicon
import Control.Coroutine (cotransform)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans (lift)
import Control.Error.Util (hoistMaybe)
import Data.Foldable (fold)
import Data.Lens (LensP, over, lens, PrismP, prism')
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as S
import Data.Tuple (Tuple(..))
import Libs.PouchDB (POUCHDB, PouchDB, PouchDBAff)
import Libs.PouchDB.Json (tryGetJson, putJson)
import Network.HTTP.Affjax (AJAX)
import Prelude
import React (ReactElement, ReactClass, createElement, createClass, transformState, getProps) as R
import React.DOM (text, div) as R
import React.DOM.Props as RP
import Thermite as T


type Props =
  { onedriveToken :: String
  , db :: Maybe PouchDB
  }


type State =
  { booksModal :: ChooseDirectoryModal.State
  , booksDir :: Maybe DirectoryInfo
  , readModal :: ChooseDirectoryModal.State
  , readDir :: Maybe DirectoryInfo
  , stateLoaded :: Boolean
  }


type DirectoryInfo =
  { itemId :: Maybe String
  , path :: String
  }


booksModal :: LensP State ChooseDirectoryModal.State
booksModal =
  lens _.booksModal (_ { booksModal = _ })


booksDir :: LensP State (Maybe DirectoryInfo)
booksDir =
  lens _.booksDir (_ { booksDir = _ })


readModal :: LensP State ChooseDirectoryModal.State
readModal =
  lens _.readModal (_ { readModal = _ })


readDir :: LensP State (Maybe DirectoryInfo)
readDir =
  lens _.readDir (_ { readDir = _ })


defaultState :: State
defaultState =
  { booksModal: ChooseDirectoryModal.defaultState
  , readModal: ChooseDirectoryModal.defaultState
  , stateLoaded: false
  , booksDir: Nothing
  , readDir: Nothing
  }


data Action
  = BooksDirectory ChooseDirectoryModal.Action
  | ReadDirectory ChooseDirectoryModal.Action
  | Save


booksDirectoryAction :: PrismP Action ChooseDirectoryModal.Action
booksDirectoryAction =
  prism' BooksDirectory getBooksDirectory
  where
    getBooksDirectory (BooksDirectory x) = Just x
    getBooksDirectory _ = Nothing


readDirectoryAction :: PrismP Action ChooseDirectoryModal.Action
readDirectoryAction =
  prism' ReadDirectory getBooksDirectory
  where
    getBooksDirectory (ReadDirectory x) = Just x
    getBooksDirectory _ = Nothing


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
          , onClick : dispatch $ BooksDirectory ChooseDirectoryModal.ShowModal
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


spec :: forall eff. T.Spec (ajax :: AJAX, err :: EXCEPTION, pouchdb :: POUCHDB | eff) State Props Action
spec =
  fold
  [ T.withState (\st ->
                  if not st.stateLoaded
                  then AjaxLoader.spec
                  else
                    case st.booksDir of
                      Nothing ->
                        chooseButton
                      Just _ ->
                        wrapMiddle $ mapPropsWithState tryGetChoosedDirectoryProps $ maybeProps $ T.match booksDirectoryAction $ ChoosedDirectory.spec
                )
  , T.simpleSpec performAction T.defaultRender
  , mapProps tryGetChooseDirectoryModalProps $ maybeProps $ T.focus booksModal booksDirectoryAction ChooseDirectoryModal.spec
  , mapProps tryGetChooseDirectoryModalProps $ maybeProps $ T.focus readModal readDirectoryAction ChooseDirectoryModal.spec
  ]
  where
    tryGetChoosedDirectoryProps p s = do
      dir <- s.booksDir
      db <- p.db
      pure { directoryPath: dir.path
           }

    tryGetChooseDirectoryModalProps p = do
      db <- p.db
      pure { onedriveToken: p.onedriveToken
           , db
           }

    performAction (BooksDirectory (ChooseDirectoryModal.FileTreeAction action)) props state =
      processFileTreeAction1 action props state
    performAction (ReadDirectory (ChooseDirectoryModal.FileTreeAction action)) props state =
      processFileTreeAction2 action props state
    performAction _ _ _ =
      pure unit

    processFileTreeAction1 action props state =
      case FileTree.unwrapChildAction action of
        Tuple itemId FileTree.SelectDirectory -> do
          void $ cotransform $ \s -> s { booksDir = Nothing, stateLoaded = false }
          dir <- lift $ getDirectoryInfo props.onedriveToken itemId
          void $ cotransform $ \s -> s { booksDir = Just dir, stateLoaded = true }
        _ ->
          pure unit

    processFileTreeAction2 action props state =
      case FileTree.unwrapChildAction action of
        Tuple itemId FileTree.SelectDirectory -> do
          void $ cotransform $ \s -> s { readDir = Nothing, stateLoaded = false }
          dir <- lift $ getDirectoryInfo props.onedriveToken itemId
          void $ cotransform $ \s -> s { readDir = Just dir, stateLoaded = true }
        _ ->
          pure unit


reactClass :: R.ReactClass Props
reactClass =
  R.createClass reactSpec.spec { componentDidMount = componentDidMount }
  where
    reactSpec =
      T.createReactSpec spec defaultState

    componentDidMount this = void $ launchAff $ do
      props <- liftEff $ R.getProps this
      void $ runMaybeT $ do
        db <- hoistMaybe props.db
        BooksDirectoryInfo info <- lift (tryGetJson db booksDirectoryInfoId) >>= hoistMaybe
        dir1 <- lift $ getDirectoryInfo props.onedriveToken info.booksItemId
        dir2 <- lift $ getDirectoryInfo props.onedriveToken info.readItemId
        liftEff $ R.transformState this (_ { booksDir = Just dir1, readDir = Just dir2, stateLoaded = true })


booksDirectory :: Props -> R.ReactElement
booksDirectory props =
  R.createElement reactClass props []


getDirectoryInfo :: forall e. String -> Maybe String -> Aff (ajax :: AJAX | e) DirectoryInfo
getDirectoryInfo token itemId = do
  item <- getItem <$> getOneDriveItem token itemId
  let parentPath = fromMaybe "" (getPath <$> item.parentReference)
  pure { itemId
       , path : parentPath <> "/" <> item.name
       }
  where
    getItem (OneDriveItem item) = item
    getPath (ItemReference reference) =
      let
        idx = S.indexOf ":" reference.path
      in
       maybe reference.path (\i -> S.drop (i + 1) reference.path) idx


updateBooksDirectoryIfNeeded :: forall e. PouchDB -> Maybe String -> PouchDBAff e Unit
updateBooksDirectoryIfNeeded db itemId = do
  BooksDirectoryInfo current <- fromMaybe defaultBooksDirectoryInfo <$> tryGetJson db booksDirectoryInfoId
  when (current.booksItemId /= itemId) $
    putJson db $ BooksDirectoryInfo current { booksItemId = itemId }
