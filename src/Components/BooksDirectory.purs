module Components.BooksDirectory (booksDirectory) where


import Common.Data.BooksDirectoryInfo (BooksDirectoryInfo(BooksDirectoryInfo), booksDirectoryInfoId, defaultBooksDirectoryInfo)
import Common.OneDriveApi (ItemReference(..), OneDriveItem(..), getOneDriveItem)
import Common.React (maybeProps, mapProps)
import Components.AjaxLoader.AjaxLoader as AjaxLoader
import Components.ChooseDirectoryModal as ChooseDirectoryModal
import Components.OneDriveFileTree as FileTree
import Components.Wrappers.Button as Button
import Components.Wrappers.Col as Col
import Components.Wrappers.ControlLabel as ControlLabel
import Components.Wrappers.Form as Form
import Components.Wrappers.FormControl as FormControl
import Components.Wrappers.FormGroup as FormGroup
import Components.Wrappers.Panel as Panel
import Control.Coroutine (cotransform)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.Error.Util (hoistMaybe)
import Data.Foldable (fold)
import Data.Lens (Lens', over, lens, Prism', prism', set, _Just, (^.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.String as S
import Data.Tuple (Tuple(..))
import Libs.PouchDB (POUCHDB, PouchDB, PouchDBAff)
import Libs.PouchDB.Json (tryGetJson, putJson)
import Network.HTTP.Affjax (AJAX)
import Prelude
import React (ReactElement, ReactClass, createElement, createClass, transformState, getProps) as R
import React.DOM (text, h3) as R
import Thermite as T


type Props =
  { onedriveToken :: String
  , db :: Maybe PouchDB
  }


type State =
  { booksDir :: Maybe DirectoryInfo
  , readDir :: Maybe DirectoryInfo
  , stateLoaded :: Boolean
  , booksModal :: ChooseDirectoryModal.State
  , readModal :: ChooseDirectoryModal.State
  }


booksDir :: Lens' State (Maybe DirectoryInfo)
booksDir =
  lens _.booksDir (_ { booksDir = _ })


readDir :: Lens' State (Maybe DirectoryInfo)
readDir =
  lens _.readDir (_ { readDir = _ })


stateLoaded :: Lens' State Boolean
stateLoaded =
  lens _.stateLoaded (_ { stateLoaded = _ })


booksModal :: Lens' State ChooseDirectoryModal.State
booksModal =
  lens _.booksModal (_ { booksModal = _ })


readModal :: Lens' State ChooseDirectoryModal.State
readModal =
  lens _.readModal (_ { readModal = _ })


type DirectoryInfo =
  { itemId :: Maybe String
  , path :: String
  }


itemId_ :: Lens' DirectoryInfo (Maybe String)
itemId_ =
  lens _.itemId (_ { itemId = _ })


defaultState :: State
defaultState =
  { stateLoaded: false
  , booksDir: Nothing
  , readDir: Nothing
  , booksModal: ChooseDirectoryModal.defaultState
  , readModal: ChooseDirectoryModal.defaultState
  }


data Action
  = BooksModalAction ChooseDirectoryModal.Action
  | ReadModalAction ChooseDirectoryModal.Action
  | Save


booksModalAction :: Prism' Action ChooseDirectoryModal.Action
booksModalAction =
  prism' BooksModalAction getModal
  where
    getModal (BooksModalAction x) = Just x
    getModal _ = Nothing


readModalAction :: Prism' Action ChooseDirectoryModal.Action
readModalAction =
  prism' ReadModalAction getModel
  where
    getModel (ReadModalAction x) = Just x
    getModel _ = Nothing


spec :: forall eff. T.Spec (ajax :: AJAX, pouchdb :: POUCHDB, err :: EXCEPTION | eff) State Props Action
spec =
  fold
  [ T.withState $ \st ->
                    if not st.stateLoaded
                    then
                      AjaxLoader.spec
                    else
                      wrapPanel (R.h3 [] [ R.text "Settings" ]) $ wrapForm $ booksDirectoryGroup <> readDirectoryGroup <> saveButton
  , T.simpleSpec performAction T.defaultRender
  , mapProps tryGetChooseDirectoryModalProps $ maybeProps $ T.focus booksModal booksModalAction ChooseDirectoryModal.spec
  , mapProps tryGetChooseDirectoryModalProps $ maybeProps $ T.focus readModal readModalAction ChooseDirectoryModal.spec
  ]
  where
    booksDirectoryGroup =
      directoryGroup "booksDirectory" "Books directory" "Books directory placeholder" _.booksDir BooksModalAction

    readDirectoryGroup =
      directoryGroup "readDirectory" "Read directory" "Read directory placeholder" _.readDir ReadModalAction

    directoryGroup controlId labelText placeholderText dirAccessor actionConstructor =
      wrapFormGroup (Just controlId) $ T.simpleSpec T.defaultPerformAction render
      where
        render dispatch _ s _ =
          [ Col.col { lg: 2, componentClass: ControlLabel.controlLabelFFI } [ R.text labelText ]
          , Col.col { lg: 2 } [ renderDirectoryInfo placeholderText (dirAccessor s) ]
          , Col.col { lg: 8 } [ Button.button
                                { bsStyle: "link"
                                , onClick: dispatch (actionConstructor ChooseDirectoryModal.ShowModal)
                                }
                                [ R.text "Choose directory" ]
                              ]
          ]

    saveButton =
      T.simpleSpec T.defaultPerformAction render
      where
        render dispatch _ _ _ =
          [ Col.col
            { lgOffset: 2, lg: 10 }
            [ Button.button { type: "submit", onClick: dispatch Save } [ R.text "Save" ] ]
          ]

    performAction Save props state =
      void $ runMaybeT $ do
        db <- hoistMaybe props.db
        let
          booksItemId =
            state ^. (booksDir <<< _Just <<< itemId_)
          readItemId =
            state ^. (readDir <<< _Just <<< itemId_)
        lift $ lift $ updateBooksDirectoryIfNeeded db booksItemId readItemId
    performAction (BooksModalAction (ChooseDirectoryModal.FileTreeAction action)) props _ =
      case FileTree.unwrapChildAction action of
        Tuple itemId FileTree.SelectDirectory ->
          updateItemId props booksDir itemId
        _ ->
          pure unit
    performAction (ReadModalAction (ChooseDirectoryModal.FileTreeAction action)) props _ =
      case FileTree.unwrapChildAction action of
        Tuple itemId FileTree.SelectDirectory ->
          updateItemId props readDir itemId
        _ ->
          pure unit
    performAction _ _ _ =
      pure unit

    updateItemId props dirSetter itemId = do
      void $ cotransform $ set dirSetter Nothing <<< set stateLoaded false
      dir <- lift $ getDirectoryInfo props.onedriveToken itemId
      void $ cotransform $ set dirSetter (Just dir) <<< set stateLoaded true

    tryGetChooseDirectoryModalProps p = do
      db <- p.db
      pure { onedriveToken: p.onedriveToken
           , db
           }

    renderDirectoryInfo placeholder maybeDirInfo =
      FormControl.formControlStatic {} [ R.text (maybe placeholder _.path maybeDirInfo) ]


wrapPanel :: forall eff state props action. R.ReactElement -> T.Spec eff state props action -> T.Spec eff state props action
wrapPanel header =
  over T._render wrapRender
  where
    wrapRender origRender dispatch p s c =
      [ Panel.panel
        { header: header }
        (origRender dispatch p s c)
      ]


wrapForm :: forall eff state props action. T.Spec eff state props action -> T.Spec eff state props action
wrapForm =
  over T._render wrapRender
  where
    wrapRender origRender dispatch p s c =
      [ Form.form
        { horizontal: true, action: "#" }
        (origRender dispatch p s c)
      ]


wrapFormGroup :: forall eff state props action. Maybe String -> T.Spec eff state props action -> T.Spec eff state props action
wrapFormGroup controlId =
  over T._render wrapRender
  where
    wrapRender origRender dispatch p s c =
      let
        fg =
          maybe (FormGroup.formGroup {}) (\c -> FormGroup.formGroup { controlId: c }) controlId
      in
        [ fg $ origRender dispatch p s c ]


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
        BooksDirectoryInfo info <- lift $ getBooksDirectoryInfo db
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
        idx = S.indexOf (wrap ":") reference.path
      in
       maybe reference.path (\i -> S.drop (i + 1) reference.path) idx


getBooksDirectoryInfo :: forall e. PouchDB -> PouchDBAff e BooksDirectoryInfo
getBooksDirectoryInfo db =
  fromMaybe defaultBooksDirectoryInfo <$> tryGetJson db booksDirectoryInfoId


updateBooksDirectoryIfNeeded :: forall e. PouchDB -> Maybe String -> Maybe String -> PouchDBAff e Unit
updateBooksDirectoryIfNeeded db booksItemId readItemId = do
  BooksDirectoryInfo current <- getBooksDirectoryInfo db
  when (current.booksItemId /= booksItemId || current.readItemId /= readItemId) $
    putJson db $ BooksDirectoryInfo current { booksItemId = booksItemId
                                            , readItemId = readItemId
                                            }
